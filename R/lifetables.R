
#' lt_flexible
#' @description Calculate an abridged-age or a single-age lifetable.
#' @param Deaths numeric vector. Death counts in one, five or ten years age groups.
#' @param Exposures numeric vector. Population counts in one, five or ten years age groups.
#' @param Age numeric vector. Age in integer years at the beginning of the interval. e.g. interval `"5-10"`, should be coded as 5
#' @param OAnew integer. Desired open age group (5-year ages only). Default `100`. If higher then rates are extrapolated.
#' @param age_out character. Indicates weather single of abridged lif table output is desired. takes values `"single"`, `"abridged"`. Defaults to "single"
#' @param extrapFrom integer. Age from which to impute extrapolated mortality.
#' @param extrapFit integer vector. Ages to include in model fitting. Defaults to all ages > =60.
#' @param radix numeric. Lifetable radix, `l0`. Default `100000`.
#' @param extrapLaw character. If extrapolating, which parametric mortality law should be invoked? Options include `"Kannisto"`, `"Kannisto_Makeham"`, `"Makeham"`, `"Gompertz"`, `"GGompertz"`, `"Beard"`, `"Beard_Makeham"`, `"Quadratic"`. Default `"Kannisto"` if the highest age is at least 90, otherwise `"makeham"`.
#' @param SRB numeric. the sex ratio at birth (boys / girls), default `1.05`
#' @param a0rule character. Either `"ak"` (default) or `"cd"`.
#' @param axmethod character. Either `"pas"` or `"un"`.
#' @param Sex character. Either `"m"` for males, `"f"` for females (default). 
#' @return A single or abridged life table iof data.frame format with corresponding columns:
#' Age integer. Lower bound of abridged age class,
#' AgeInt integer. Age class widths.
#' nMx numeric. Age-specific central death rates.
#' nAx numeric. Average time spent in interval by those deceased in interval.
#' nqx numeric. Age-specific conditional death probabilities.
#' lx numeric. Lifetable survivorship
#' ndx numeric. Lifetable deaths distribution.
#' nLx numeric. Lifetable exposure.
#' Sx numeric. Survivor ratios in uniform 5-year age groups.
#' Tx numeric. Lifetable total years left to live above age x.
#' ex numeric. Age-specific remaining life expectancy.
#' @importFrom dplyr case_when
#' @importFrom DemoTools lt_abridged age2int lt_abridged2single lt_single_mx lt_single2abridged
#' @export
#' @examples
#' \dontrun{
#' Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
#' 247473,223014,172260,149338,127242,105715,79614,53660,
#' 31021,16805,8000,4000,2000,1000)
#' 
#' Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
#'             1335,3257,2200,4023,2167,4578,2956,4212,
#'             2887,2351,1500,900,500,300)
#'
#'Age = c(0, 1, seq(5, 100, by = 5))
#' data_out <- 
#'   lt_flexible(Deaths    = Deaths, 
#'               Exposures = Exposures,
#'               Age       = Age,
#'               OAnew     = 100,
#'               age_out   = "single",  
#'               extrapFrom = 80,
#'               extrapFit = Age[Age >= 60], 
#'               radix     = 1e+05,
#'               extrapLaw = NULL,
#'               SRB       = 1.05,
#'               a0rule    = "ak",
#'               axmethod  = "un",
#'               Sex       = "m")
#' }
lt_flexible <- function(Deaths     = Deaths, # replace with NULL. this is for demonstration purposes
                        Exposures  = Exposures,
                        Age        = Age,
                        # recall all of these are passed in from the app, which will contain
                        # its own default values.
                        OAnew      = 100,
                        age_out    = "single", # CHECK! This is for option number 2 
                        extrapFrom = 80,
                        extrapFit  = Age[Age >= 60],
                        extrapLaw  = NULL,
                        radix      = 1e+05,
                        SRB        = 1.05,
                        a0rule     = "ak",
                        axmethod   = "un",
                        Sex        = "m") {
  # TR: no need to determine extrapLaw here, it happens
  # natively in the lt functions.
  
  
  
  age_in <- case_when(is_single(Age)  ~ "single",
                      is_abridged(Age) ~ "abridged",
                      TRUE ~ "problem")
  
  # TR: this can become the checker function I guess
  if (age_in == "problem"){
    stop(
      "Age groups appear irregular. Only single or standard abrdiged ages are supported now"
    )
  }
  
  # age_in and out_out both abridged
  if(age_in == "abridged" & age_out == "abridged") {
    # TR possibly more args to pass, or different arg management;
    # for instance, construct a completed list of args
    # and execute the function using do.call()
    AgeInt <- age2int(Age)
    
    data_out <- lt_abridged(Deaths  = Deaths,
                            Exposures  = Exposures,
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
  
  # age_in abridged and age_out single
  if (age_in == "abridged" & age_out == "single") {
    
    data_out <- lt_abridged2single(Deaths     = Deaths,
                                   Exposures  = Exposures,
                                   Age        = Age,
                                   # AgeInt     = AgeInt,
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
  
  # age_in single, calculate no matter whether we 
  # keep single ages or want abridged output; if single
  # data_out doesn't change again
  if (age_in == "single"){
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
  
  # final case, age_in single and age_out abridged,
  # requires the precalculated single-age lifetable
  if (age_in == "single" & age_out == "abridged") {
    
    data_out <- lt_single2abridged(lx  = data_out$lx, 
                                   nLx = data_out$nLx, 
                                   ex  = data_out$ex, 
                                   Age = data_out$Age)
    
  }
  
  # now all cases handled
  return(data_out)
  
}