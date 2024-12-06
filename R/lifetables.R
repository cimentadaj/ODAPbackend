#' @title `lt_flexible`
#' @description Wrapper for calculation of abridged-age or single-age lifetable.
#' @param data_in data.frame or tibble. Should contain numeric columns `Age`, `Deaths`, and `Exposures`, or `nMx` or any of the following lifetable functions: `nqx`, `ndx`, `lx`. Can have a `.id` column in which case the results will be generated for each group as specified by this column.
#' @param OAnew integer. Desired open age group (5-year ages only). Default to `100`. If higher, then rates will be extrapolated.
#' @param age_out character. Indicates whether single or abridged lifetable output is desired. Takes values `single`, or `abridged`. Defaults to `single`.
#' @param extrapFrom integer. Age from which to impute extrapolated mortality.
#' @param extrapFit integer vector. Ages to include in model fitting. Defaults to all ages >= 60.
#' @param radix numeric. Lifetable radix, `l(0)`. Defaults to `100000`.
#' @param extrapLaw character. If extrapolating, which parametric mortality law should be invoked? Options include `"Kannisto"`, `"Kannisto_Makeham"`, `"Makeham"`, `"Gompertz"`, `"GGompertz"`, `"Beard"`, `"Beard_Makeham"`, `"Quadratic"`. Defaults to `Kannisto` if the highest age is at least 90, otherwise o `Makeham`.
#' @param SRB numeric. The sex ratio at birth (boys/girls). Defaults to `1.05`.
#' @param a0rule character. Rule for `a(x)` calculation Either `ak` (default) or `cd`.
#' @param axmethod character. Method used for `a(x)` calculation. Either `pas` or `un`.
#' @param Sex character. Either `m` for males, `f` for females or `t` for total. This variable defaults to `t`. If there is more than one sex in the data, then the lifetable will be calculated for each sex.
#' @param by_args character. A vector of columns should be also included in the output. These columns are usually ones that are used for `.id` construction. Defaults to `NULL`. It is important to not include `Sex` in this vector.
#' @importFrom dplyr mutate filter reframe first bind_rows ungroup .data case_match
#' @importFrom purrr map_lgl
#' @importFrom tidyselect all_of
#' @importFrom tidyr unnest
#' @importFrom DemoTools age2int lt_id_d_q lt_id_l_q
#' @importFrom rlang set_names .data
#' @return A list with 3 elements: `data_out` - single or abridged life table of data.frame format with corresponding columns:
#' Age integer. Lower bound of abridged age class,
#' AgeInt integer. Age class widths.
#' nMx numeric. Age-specific central death rates.
#' nAx numeric. Average time spent in the interval by those deceased.
#' nqx numeric. Age-specific conditional death probabilities.
#' lx numeric. Lifetable survivorship
#' ndx numeric. Lifetable deaths distribution.
#' nLx numeric. Lifetable exposure.
#' Sx numeric. Survivor ratios in uniform 5-year age groups.
#' Tx numeric. Lifetable total years left to live above age x.
#' ex numeric. Age-specific remaining life expectancy.
#' .id character A group indicator for which the results will be generated. In case of missing the .id column will return `all`
#' Sex - corresponding sex
#' by_args - chosen additional arguments
#' `arguments` - a list of arguments used in fitting `lt_flixible` function, `arguments2` - a list of arguments used for `lt_flexible_chunk` fitting. 
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' # single age data
# data_in <- read_csv("inst/extdata/single_hmd_spain.csv") |>
#   dplyr::select(-1)
# 
# data_out <- lt_flexible(
#   data_in,
#   OAnew      = 100,
#   age_out    = "single",
#   extrapFrom = 80,
#   extrapFit  = NULL,  # Default NULL, computed later
#   extrapLaw  = NULL,
#   radix      = 1e+05,
#   SRB        = 1.05,
#   a0rule     = "Andreev-Kingkade",
#   axmethod   = "UN (Greville)",
#   Sex = "t"
# )
# data_out$data_out
# data_out$arguments2
# data_out$arguments
  
lt_flexible <- function(data_in,
                        OAnew      = 100,
                        age_out    = "single",
                        extrapFrom = 80,
                        extrapFit  = NULL,  # Default NULL, computed later
                        extrapLaw  = NULL,
                        radix      = 1e+05,
                        SRB        = 1.05,
                        a0rule     = "Andreev-Kingkade",
                        axmethod   = "UN (Greville)",
                        Sex = "t",
                        by_args = NULL) {
  
  if (!"Sex" %in% colnames(data_in)){
    data_in$Sex <- Sex
  }
  
  data_in <- data_in |>
    mutate(Sex = substr(Sex, 1, 1),
           Sex = ifelse(Sex == "t", "b", Sex),
           Sex = tolower(Sex))

  # match potential names. In case raw data is supplied
  nms <- case_match(
    names(data_in),
    c("Deaths", "deaths") ~ "Deaths",
    c("Population", "population",
      "Exposures", "Exp") ~ "Exposures",
    c("nMx", "mx", "Mx")  ~ "nMx",
    c("lx", "nlx")        ~ "lx",
    c("dx", "ndx")        ~ "ndx",
    c("qx", "nqx")        ~ "nqx",
    c("px", "npx")        ~ "npx",
    .default = names(data_in)
  )
  
  # fix names
  names(data_in) <- nms
  
  if (!(".id" %in% colnames(data_in))) {
    data_in <- data_in |>
      mutate(.id = "all")
  }
  
  # Set extrapFit here, avoiding circular reference in defaults
  if (is.null(extrapFit)) {
    extrapFit <- unique(data_in$Age)[unique(data_in$Age) >= 60]
  }
  
 # by_args <- names(data_in)[!names(data_in) %in% c("Age", "Deaths", "Exposures",
 #                                                   "Mx_emp", "Rates", "AgeInt")]

 # simplest case - build from existing nMx
  if("nMx" %in% names(data_in)) {
    
    data_in <- data_in |> 
      mutate(Mx_emp = .data$nMx)
    
  } 
  
  # if both deaths and Exposures are present calculate nMx and build LT
  if(!"nMx" %in% names(data_in) & all(c("Deaths", "Exposures") %in% names(data_in))) { 
    
    data_in <- data_in |>
      mutate(Mx_emp = .data$Deaths / .data$Exposures)
    
  }
  
  # calculate nMx from nqx
  # All other functions first turn LT function into nqx and then use the formula for nqx to nMx
  # if("nqx" %in% names(data_in)) {
  #   
  #   data_in <- data_in |> 
  #     group_by(.data$.id) |>
  #     mutate(
  #       AgeInt = age2int(.data$Age, OAvalue = 5),
  #       Mx_emp = .data$nqx / (.data$AgeInt - .data$nqx * (.data$AgeInt / 2))
  #       )
  # } 
  
  # from survival probabilities
  # if("npx" %in% names(data_in)) {
  #   
  #   data_in <- data_in |> 
  #     group_by(.data$.id) |>
  #     mutate(AgeInt = age2int(.data$Age, OAvalue = 5),
  #            nqx    =  1 - .data$nqx,
  #            Mx_emp = .data$nqx / (.data$AgeInt - .data$nqx * (.data$AgeInt / 2))
  #            )
  # } 
  # 
  # from ndx - decrement
  if("ndx" %in% names(data_in) & !"nMx" %in% names(data_in) & all(c("Deaths", "Exposures") %in% names(data_in))) {
    
    data_in <-  data_in |> 
      group_by(.data$.id) |>
      mutate(nqx = lt_id_d_q(.data$ndx))
    
  } 
  
  # from survaval function
  if("lx" %in% names(data_in) & !"ndx" %in% names(data_in) & !"nMx" %in% names(data_in) & all(c("Deaths", "Exposures") %in% names(data_in))) {
    
    data_in <-  data_in |> 
      group_by(.data$.id) |>
      mutate(nqx = lt_id_l_q(.data$lx))
  }
  
  data_out <- data_in |>
    ungroup() |> 
    reframe(
      lt_flexible_chunk(data_in    = .data, 
                        Sex        = first(Sex),  
                        OAnew      = OAnew,  
                        extrapFrom = extrapFrom,
                        extrapFit  = extrapFit, 
                        radix      = radix,
                        extrapLaw  = extrapLaw,
                        SRB        = SRB,
                        a0rule     = a0rule,
                        axmethod   = axmethod,
                        age_out    = age_out

      ), .by = all_of(c(".id"))
    ) |>
    set_names(c(".id", by_args, "data"))
  
  data <- data_out |>
    filter(map_lgl(data, is.data.frame))|>
    unnest(.data$data)
  
  args <- data_out |>
    filter(map_lgl(data, ~ !is.data.frame(.))) |>
    bind_rows(.id = ".id")
  
  return(list(data_out   = data))
}

# [ ] allow lx, nMx, nqx inputs
# [ ] if data go up to 75+ we can't have jumpoff at 80, the value 80 needs
#     to be dynamically determined

#' @title `lt_flexible_chunk`
#' @description Calculate an abridged or a single-age lifetable.
#' @param data_in a `data.frame` or `tibble`. 3 numeric columns `Age`, `Deaths`, and `Exposures`, or `nMx` or any of the following lifetable functions: `nqx`, `npx`, `ndx`, `lx`. Can optionally have columns `Sex` and `.id` in which case the table will be calculated for each level in these columns. Can also have an additional column.
#' @param OAnew integer. Desired open age group (5-year ages only). Default `100`. If higher then rates are extrapolated.
#' @param age_out character. Indicates whether single or abridged lifetable output is desired. takes values `single`, `abridged`. Defaults to `single`.
#' @param extrapFrom integer. Age from which to impute extrapolated mortality. Defaults to `NULL`.
#' @param extrapFit integer vector. Ages to include in model fitting. Defaults to all ages >= 60.
#' @param radix numeric. Lifetable radix, `l(0)`. Default `100000`.
#' @param extrapLaw character. If extrapolating, which parametric mortality law should be invoked? Options include `Kannisto`, `Kannisto_Makeham`, `Makeham`, `Gompertz`, `GGompertz`, `Beard`, `Beard_Makeham`, `Quadratic`. Defaults to `Kannisto` if the highest age is at least 90, otherwise to `Makeham`.
#' @param SRB numeric. the sex ratio at birth (boys/girls). Defaults to `1.05`.
#' @param a0rule character. An a(0) calculation rule. Either `ak` (default) or `cd`.
#' @param axmethod character. A method used for a(0) calculation. Either `pas` or `un`.
#' @param Sex character. Either `m` for males, `f` for females, or `t` for total (default).
#' @param age_out character. Indicates whether single or abridged lifetable output is desired. Takes values `single`, or `abridged`. Defaults to `single`.
#' @return A list with two elements: A single or abridged life table of data.frame format with corresponding columns:
#' Age integer. Lower bound of abridged age class,
#' AgeInt integer. Age class widths.
#' nMx numeric. Age-specific central death rates.
#' nAx numeric. Average time spent in the interval by those deceased.
#' nqx numeric. Age-specific conditional death probabilities.
#' lx numeric. Lifetable survivorship.
#' ndx numeric. Lifetable deaths distribution.
#' nLx numeric. Lifetable exposure.
#' Sx numeric. Survivor ratios in uniform 5-year age groups.
#' Tx numeric. Lifetable total years left to live above age x.
#' ex numeric. Age-specific remaining life expectancy.
#' Sex character. Sex.
#' @importFrom dplyr case_when case_match mutate
#' @importFrom DemoTools is_single lt_abridged age2int lt_abridged2single lt_single_mx lt_single2abridged is_abridged lt_id_d_q lt_id_l_q
#' `arguments` - a list of arguments used in fitting.
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' # single age data
#' data_in <- read_csv("inst/extdata/single_hmd_spain.csv") |>
#'   dplyr::select(-1) |>
#'   filter(.id == 1)
#' 
#' data_out <-
#'   lt_flexible_chunk(data_in,
#'                     OAnew     = 100,
#'                    age_out   = "abridged",
#'                    extrapFrom = 80,
#'                    # extrapFit = Age[Age >= 60],
#'                    radix     = 1e+05,
#'                     extrapLaw = NULL,
#'                     SRB       = 1.05,
#'                    a0rule    = "ak",
#'                    axmethod  = "un",
#'                    Sex       = "f")
#' 
#' data_out$data_out
#' data_out$arguments
#' 
#' 
lt_flexible_chunk <- function(
    data_in,
    Sex,
    OAnew      = 100,
    age_out    = "single", 
    extrapFrom = 80,
    extrapFit  = NULL,  # No circular references
    extrapLaw  = NULL,
    radix      = 1e+05,
    SRB        = 1.05,
    a0rule     = "Andreev-Kingkade",
    axmethod   = "UN (Greville)") {
  
  Age    <- data_in$Age
  AgeInt <- age2int(Age, OAvalue = 5)
  # Mx_emp <- data_in$Mx_emp
  existsMx_emp <- !is.null(tryCatch(data_in$Mx_emp, error = function(e) NULL))
  
  if(existsMx_emp) {
    
    Mx_emp <- data_in$Mx_emp
    nqx    <- NULL
    
  } else {
    
    Mx_emp <- NULL
    nqx    <- data_in$nqx
    
  }
  
  a0rule <- case_match(a0rule,
                       "Andreev-Kingkade"  ~ "ak",
                       "Coale-Demeny"      ~ "cd",
                       .default = a0rule)
  
  axmethod <- case_match(axmethod,
                         "UN (Greville)" ~ "un",
                         "PASEX"         ~ "pas",
                         .default = axmethod)
  
  age_in <- case_when(is_single(Age)   ~ "single",
                      is_abridged(Age) ~ "abridged",
                      TRUE             ~ "problem")
  
  # TR: this can become the checker function I guess
  if (age_in == "problem") {
    stop(
      "Age groups appear irregular. Only single or standard abrdiged ages are supported now"
    )
  }
  
  if (age_in == "abridged" & age_out == "abridged") {
    
    # TR possibly more args to pass, or different arg management;
    # for instance, construct a completed list of args
    # and execute the function using do.call()
    
    data_out <- lt_abridged(nMx        = Mx_emp,
                            nqx        = nqx,
                            Age        = Age,
                            AgeInt     = AgeInt,
                            OAnew      = OAnew,
                            extrapFrom = extrapFrom,
                            extrapFit  = extrapFit,
                            radix      = radix,
                            extrapLaw  = extrapLaw,
                            SRB        = SRB,
                            a0rule     = a0rule,
                            axmethod   = axmethod,
                            Sex        = Sex)
  }
  
  # Compute `data_out` based on age conditions
  if (age_in == "abridged" & age_out == "single") {
    
    data_out <- lt_abridged2single(nMx        = Mx_emp,
                                   nqx        = nqx,
                                   Age        = Age,
                                   OAnew      = OAnew,  
                                   extrapFrom = extrapFrom,
                                   extrapFit  = extrapFit,
                                   radix      = radix,
                                   extrapLaw  = extrapLaw,
                                   SRB        = SRB,
                                   a0rule     = a0rule,
                                   axmethod   = axmethod,
                                   Sex        = Sex)
  }
  
  if(age_in == "single") {
    
    if(!is.null(Mx_emp)) {
      
    # useful in case we use lt_single_mx()
    # Don't check age_out yet here, because the abridge function requires a
    # precalculated lifetable, see below
    # TR same story; arg management should be complete and strategic
    data_out <- lt_single_mx(nMx        = Mx_emp,
                             Age        = Age,
                             OAnew      = OAnew,
                             extrapFrom = extrapFrom,
                             extrapFit  = extrapFit, # should we change it here too to 1 year intervals?
                             extrapLaw  = extrapLaw,
                             radix      = radix,
                             SRB        = SRB,
                             a0rule     = a0rule,
                             axmethod   = axmethod,
                             Sex        = Sex)
    
    } 
    
    if(!is.null(nqx)) {
      
      data_out <- lt_single_qx(
                           nqx        = nqx,
                           Age        = Age,
                           OAnew      = OAnew,
                           extrapFrom = extrapFrom,
                           extrapFit  = extrapFit, # should we change it here too to 1 year intervals?
                           extrapLaw  = extrapLaw,
                           radix      = radix,
                           SRB        = SRB,
                           a0rule     = a0rule,
                           axmethod   = axmethod,
                           Sex        = "m")
    }
  }
  
  if(age_in == "single" & age_out == "abridged") {
    
    data_out <- lt_single2abridged(lx  = data_out$lx,
                                   nLx = data_out$nLx,
                                   ex  = data_out$ex,
                                   Age = data_out$Age)
    
  }
  
  Sex <- case_match(Sex,
                    "m" ~ "Males",
                    "f" ~ "Females",
                    "b" ~ "Total",
                    "t" ~ "Total")
   
  # Add sex column to output
  data_out <- data_out |>
    mutate(Sex = Sex, .before = 1)
  
  return(list(data_out  = data_out))
  
  }

#' @ title `lt_plot`
#' @description Plot wrapper, creates lifetable plot list returned by `lt_flexible()`
#' @details This function should be run after `lt_flexible()`, so that you can pass both `data_in` and `data_out`. There is no fallback at this time to generate `data_out` on the fly if missing. `extrapFrom` should be passe dat this time to indicate the jump-off in the plot. In the future, this may be detected or passed in another way.
#' @importFrom dplyr group_split mutate group_nest full_join
#' @importFrom purrr map_lgl map2 map set_names
#' @importFrom tidyr unnest pivot_wider 
#' @export
#' @param data_in a `data.frame` or `tibble` with columns `Age`, `Deaths`, and `Exposures` and `.id`
#' @param data_out `tibble` as produced by `lt_flexible()`
#' @param extrapFrom integer. Age from which to impute extrapolated mortality.
#' library(readr)
#' library(dplyr)
#' # single age data
#' data_in <- read_csv("inst/extdata/single_hmd_spain.csv") |>
#'   dplyr::select(-1) |>
#'   filter(.id == 1)
#' 
#' data_out <- lt_flexible(
#'   data_in,
#'   OAnew      = 100,
#'   age_out    = "single",
#'   extrapFrom = 80,
#'   extrapFit  = NULL,  # Default NULL, computed later
#'   extrapLaw  = NULL,
#'  radix      = 1e+05,
#'   SRB        = 1.05,
#'   a0rule     = "Andreev-Kingkade",
#'   axmethod   = "UN (Greville)",
#'   Sex = "t"
#' )
#' 
#' ex1 <-  lt_plot(
#'   data_in = data_in,
#'   data_out = data_out,
#'   extrapFrom = 80)
#'   
#' ex1$`1`$nMx
#' ex1$`1`$lx
#' ex1$`1`$ndx
#' ex1$`1`$nqx
#' ex1$nMx

lt_plot <- function(data_in,
                    data_out, 
                    extrapFrom = extrapFrom){
  
  
  if (!(".id" %in% colnames(data_in))){
    data_in <- data_in |>
      mutate(.id = "all")
  }
  
  id1 <- unique(data_in$.id)
  
  plots <- data_out |>
    group_split(.data$.id, .keep = TRUE) |>
    map(~ plot_lifetable(.x)) |>
    set_names(id1)
  
  # sorry JC, forgot this!
  d_in <-  data_in |>
    mutate(type = "d_in") |>
    mutate(id = .data$.id) |>
    group_nest(.data$.id, .data$type)
  
  d_out <- data_out |>
    mutate(type = "d_out")|>
    mutate(id = .data$.id) |>
    group_nest(.data$.id, .data$type)
  
  data <- d_in |>
    full_join(d_out) |> 
    pivot_wider(names_from  = .data$type,
                values_from = .data$data) |>
    mutate(new = map2(.x = d_out,
                      .y = d_in, 
                      ~ plot_compare_rates(data_in  = .y,
                                           data_out = .x,
                                           extrapFrom = extrapFrom)))
  
  plots$nMx <- data$new
  
  return(plots)
  
}

# TODO: lt_summary() should create a table of useful summary statistics from the lifetable:
# e0, e65, 45q15, sd, IQR (from LifeIneq), mode (use Paola Vasquez' shorthand formula rather than spline method)
# DONE We have to think exactly what measures do we want here. The carcass is ready, changing it is a matter of minutes.
# TODO: TO finish the roxxygen description after we decide which functions we keep and on the output

#' @title `lt_summary`
#' @description Creates a table of useful summary statistics from the lifetable.
#' @param data_out a data.frame or tibble. The data.frame output of the `lt_flexible` function.
#' @return A  data.frame containing the information on the following useful lifetable statistics:
#' e0 - life expectancy at birth
#' e65 - life expectancy at age 65
#' `S[1]` - The standard deviation in age at death from birth
#' `S[11]` - The standard deviation in age at death from age 10
#' IQR1 - interquartile range survivorship age from a lifetable
#' IQR2 - interquartile range survivorship age from a lifetable
#' IQR3 - interquartile range survivorship age from a lifetable
#' mod_age - modal age at death
#' q15_45 - probability that the person ages 54 will die at age 60
#' @importFrom tibble lst tibble
#' @importFrom LifeIneq ineq_sd ineq_iqr ineq_quantile 
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr everything mutate reframe
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' # single age data
#' data_in <- read_csv("inst/extdata/single_hmd_spain.csv") |>
#'   dplyr::select(-1) |>
#'   filter(.id == 1)
#' 
#' data_out <- lt_flexible(
#'   data_in,
#'   OAnew      = 100,
#'   age_out    = "single",
#'   extrapFrom = 80,
#'   extrapFit  = NULL,  # Default NULL, computed later
#'   extrapLaw  = NULL,
#'   radix      = 1e+05,
#'   SRB        = 1.05,
#'   a0rule     = "Andreev-Kingkade",
#'   axmethod   = "UN (Greville)",
#'   Sex = "t"
#' )
#' 
#' lt_summary(data_out$data_out)

lt_summary <- function(data_out){
  
  if (!(".id" %in% colnames(data_out))){
    data_out <- data_out |>
      mutate(.id = "all")
  }
  
  out <- data_out |>
    reframe(lt_summary_chunk(data_out = .data), 
            .by = .data$.id)
  
  return(out)
}


# TODO: add column called 'label'
#' @title `lt_summary_chunk`
#' @description Creates a table of useful summary statistics from the lifetable.
#' @param data_out a data.frame or tibble. The data.frame output of the `lt_flexible` function.
#' @return A  data.frame containing the information on the following useful lifetable statistics:
#' e0 - life expectancy at birth
#' e65 - life expectancy at age 65
#' `S[1]` - The standard deviation in age at death from birth
#' `S[11]` - The standard deviation in age at death from age 10
#' IQR1 - interquartile range survivorship age from a lifetable
#' IQR2 - interquartile range survivorship age from a lifetable
#' IQR3 - interquartile range survivorship age from a lifetable
#' mod_age - modal age at death
#' q15_45 - probability that the person ages 54 will die at age 60
#' @importFrom tibble lst tibble
#' @importFrom LifeIneq ineq_sd ineq_iqr ineq_quantile 
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr everything mutate reframe
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' # single age data
#' data_in <- read_csv("inst/extdata/single_hmd_spain.csv") |>
#'   dplyr::select(-1) |>
#'   filter(.id == 1)
#' 
#' data_out <- lt_flexible(
#'   data_in,
#'   OAnew      = 100,
#'   age_out    = "single",
#'   extrapFrom = 80,
#'   extrapFit  = NULL,  # Default NULL, computed later
#'   extrapLaw  = NULL,
#'   radix      = 1e+05,
#'   SRB        = 1.05,
#'   a0rule     = "Andreev-Kingkade",
#'   axmethod   = "UN (Greville)",
#'   Sex = "t"
#' )
#' 
#' lt_summary_chunk(data_out$data_out)
lt_summary_chunk <- function(data_out) {
  
  e0  <- data_out$ex[data_out$Age == 0]
  e65 <- data_out$ex[data_out$Age == 65]
  S   <- ineq_sd(age = data_out$Age,
                 dx  = data_out$ndx,
                 ex  = data_out$ex,
                 ax  = data_out$nAx)
  

  IQR        <- ineq_iqr(age   = data_out$Age, 
                         lx    = data_out$lx, 
                         lower = 0.25,  
                         upper = 0.75)
  median_age <- ineq_quantile(age      = data_out$Age, 
                              lx       = data_out$lx, 
                              quantile = 0.5)[1]
  mod_age    <- modal_age(data_out)  
  l20        <- data_out$lx[data_out$Age == 20]
  l65        <- data_out$lx[data_out$Age == 65]
  p_20_65    <- l65 / l20
  q_20_65    <- 1 - p_20_65
  
  out <- tibble(e0, 
                Median = median_age,
                Mode   = mod_age,
                e65, 
                sd0  = S[1], 
                sd10 = S[11],
                IQR,
                q_20_65) |> 
    pivot_longer(everything(), names_to = "measure", values_to = "value") |> 
    mutate(label = c("e_0","Median","Mode","e_65","\\sigma_0","\\sigma_{10}","IQR","{}_{45}q_{20}"),
           message = c("life expectancy at birth",
                       "median age at death",
                       "modal age at death",
                       "remaining life expectancy at age 65",
                       "lifespan variation calculated as standard deviation of age at death",
                       "standard deviation of remaining lifespan conditional on survival to age 10",
                       "interquartile range of age at death distribution",
                       "conditional probability of death between ages 20 and 65"))
  
  return(out)
  
}

# helper function that calculates the modal age at death
#  Formula for mode from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3000019/ Appendix A, A2

modal_age <- function(data_out) {
  
  # we have to remove the data from the first age, since many 
  # deaths are registered at this age.
  Age      <- data_out$Age[-1]
  dx       <- data_out$ndx[-1]
  
  ind  <- which.max(dx)
  
  dx0  <- dx[ind] 
  dx1  <- dx[ind - 1] 
  dx2  <- dx[ind + 1] 
  agem <- Age[ind]
  
  agem + ((dx0 - dx1) / (dx0 - dx1 + dx0 - dx2))
  
}

#' @title `check_e0`
#' @description Creates a plot of `e(0)` values from Human Mortality Database and compare it with those obtained by user.
#' @param data_out a data.frame or tibble. The data.frame output of the `lt_flexible` function.
#' @return A  figure where the users `e(0)` values would be indicated in color and HMD values would be black:
#' @importFrom tibble lst tibble
#' @importFrom dplyr bind_rows mutate filter pull select
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_csv 
#' @importFrom ggplot2 ggplot geom_jitter geom_point theme_light geom_hline aes theme element_blank
#' @importfrom stats na.omit
#' @export
#' @examples
#' library(readr)
#' library(dplyr)
#' # single age data
#' data_in <- read_csv("inst/extdata/single_hmd_spain.csv") |>
#'   dplyr::select(-1)
#' 
#' data_out <- lt_flexible(
#'   data_in,
#'   OAnew      = 100,
#'   age_out    = "single",
#'   extrapFrom = 80,
#'   extrapFit  = NULL,  # Default NULL, computed later
#'   extrapLaw  = NULL,
#'   radix      = 1e+05,
#'   SRB        = 1.05,
#'   a0rule     = "Andreev-Kingkade",
#'   axmethod   = "UN (Greville)",
#'   Sex = "t"
#' )
#' 
#' check_e0(data_out)
#' 

check_e0 <- function(data_out) {
  
  # hmd e0 values
  e0 <- read_csv("inst/extdata/e0.csv")
  
  # our e0 values
  res <- data_out$data_out |>
    filter(.data$Age == 0) |>
    pull(.data$ex)
  
  # text comparison 
  # names(res) <- unique(x[[1]]$data_out$.id)
  # 
  # # option one
  # dat <- e0 %>% 
  #   na.omit() %>% 
  #   pivot_longer(-c(country, Year),
  #                names_to  = "sex",
  #                values_to = "val") %>%
  #   pull(val) %>% 
  #   quantile()
  # 
  # dat <- c(dat, res)
  # as.data.frame(dat,
  #               row.names = names(dat))
  
  # option 2
  
  # hmd data
  hmd <- e0 |>
    na.omit() |>
    pivot_longer(-c(.data$country, .data$Year), 
                 names_to  = "sex", 
                 values_to = "e(0)")
  
  # empirical e(0)
  emp <- data_out$data_out |> 
    filter(.data$Age == 0) |>
    dplyr::select(.data$.id, .data$ex) |>
    mutate(.id = as.factor(.data$.id))
  
  # resulting figure
  # just a cloud of points with our values in color
  fig <-  ggplot() +
    geom_jitter(data = hmd, 
                aes(x = 1, 
                    y = .data$`e(0)`), alpha = 0.3) +
    geom_point( data = emp, 
                aes(x = 1, 
                    y = .data$ex, color = .data$.id), 
                size = 3) +
    theme_light() +
    geom_hline(yintercept = 16,
               color      = "black",
               linetype   = "dashed") +
    geom_hline(yintercept = 90,
               color      = "black",
               linetype   = "dashed") +
    theme(
      legend.position = "bottom",
      axis.title.x    = element_blank(),
      axis.text.x     = element_blank()
    )
  
  return(fig)
  
}
