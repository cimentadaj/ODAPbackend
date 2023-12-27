

#' @title smooth_flexible rough and fine smoothing and graduation of count data
#' @description Smoothes population or death counts using the variaety of methods from DemoTools packgae and "Method protocol for the evaluation of census population data by age and sex" paragraph 5.
#' @param data_in tibble. A tibble with two numeric columns - population or death counts with supported names: `Pop`, `Population`, `Exp`, `Exposures` or `Deaths`, and corresponding `Age` - provided in single age intervals, 5-year age intervals, or abridged age format e.g. with ages 0, 1-4, 5-9 etc.
#' @param variable character. A scalar with the variable name which is to be graduated. The list of possible options include `Pop`, `Population`, `Exp`, `Exposures` or `Deaths`.
#' @param fine_method the `method` argument of `graduate()` function from DemoTools that graduates grouped data. Possible options include  `auto`, `none`, `sprague`, `beers(ord)`, `beers(mod)`, `grabill`, `pclm`, `mono`, `uniform`
#' @param rough_method the `method` argument of `smooth_age_5()` function from DemoTools that Smoothes populations in 5-year age groups using various methods. Possible options include `auto`, `none`, `Carrier-Farrag`, `KKN`, `Arriaga`, `United Nations`, `Strong`, `Zigzag`.
#' @param constrain_infants logical, if age 0 is a separate age class, shall we constrain its proportion within the age group 0-5 in the output? Default `TRUE`.
#' @param u5m numeric. Under five mortality rate.
#' @param age_out character. The desired age structure of the output file. Possible options include `single` - for sinle years, `5-year` - for 5-year data, and `abridged` - for abridged data, e.g 0, 1-4, 5-9, etc.
#' @param Sex character. Either `"m"` for males, `"f"` for females, or `"t"` for total (defualt).
#' @importFrom dplyr case_when mutate group_by summarize rename left_join select join_by pull
#' @importFrom tibble tibble
#' @importFrom rlang := !! sym
#' @importFrom DemoTools age2int is_single is_abridged graduate_uniform names2age calcAgeAbr groupAges smooth_age_5 graduate
#' @return data_out. A tibble with two numeric columns - smoothed counts for the chosen variable and `Age` - chosen age grouping
#' @examples
#' data(pop1m_ind, package = "DemoTools")
#' data_in <- data.frame(Exposures = pop1m_ind,
#'                       Age       = 0:100)
#'                       
#' ex1 <- smooth_flexible(
#' data_in, 
#' variable     = "Exposures",
#' rough_method = "auto",
#' fine_method  = "none", 
#' constrain_infants = TRUE, 
#' age_out = "abridged", 
#' u5m     = NULL,
#' Sex     = "t")
#'

smooth_flexible <- function(data_in,
                            variable     = "Deaths",
                            age_out      = c("single", "abridged", "5-year"),
                            fine_method  = c("auto", "none", "sprague",
                                             "beers(ord)", "beers(mod)",
                                             "grabill", "pclm", "mono",
                                             "uniform"),
                            rough_method = c("auto", "none", "Carrier-Farrag",
                                             "KKN", "Arriaga", "United Nations",
                                             "Strong", "Zigzag"),
                            u5m = NULL,
                            Sex = c("t", "f", "m"),
                            constrain_infants = TRUE) {
  
  # ensure just one of each method is chosen. 
  # rough auto is compatible with a non-auto fine,
  # since we can always regroup to 5s. 
  # Likewise pclm rough is compatible with pclm fine; # UPDATE!!! pclm removed from rough methods
  # no pclm offsets in this implementation, and no explicit tail control.
  # we exclude MAV to avoid passing in special parameters.
  # coerce to lower case for friendlier arg passing
  # frough_method <- tolower(rough_method)
  rough_method <- match.arg(rough_method, c("auto", "none", "Carrier-Farrag",
                                                    "KKN", "Arriaga", "United Nations",
                                                    "Strong", "Zigzag"))
  
  fine_method <- tolower(fine_method)
  fine_method <- ifelse(fine_method == "beers", "beers(ord)", fine_method)
  fine_method <- match.arg(fine_method, tolower(c("auto", "none", "sprague",
                                                  "beers(ord)", "beers(mod)",
                                                  "grabill", "pclm", "mono", "uniform")))
  age_out     <- match.arg(age_out, c("single", "abridged", "5-year"))
  
  # Handles e.g. Total, total, t, etc
  Sex <- substr(Sex, 1, 1) |> 
    tolower()
  Sex <- match.arg(Sex, c("t", "f", "m"))
  

  # get variables
  value       <- data_in[, variable, drop = TRUE]
  age         <- data_in$Age
  has_infants <- age2int(age)[1] == 1 & age[1] == 0
  
  # # TODO
  # if(has_infants){
  #   # get prop0
  # }
  # 
  # detect incoming age categorization
  age_in      <- case_when(is_single(age)       ~ "single",
                           is_abridged(age)     ~ "abridged",
                           all(age2int(age) == 5,
                               na.rm = TRUE)    ~ "5-year",   
                           TRUE                 ~ "other")
  
  if(age_in == "single") {
    
    data1 <- data_in
    
  }
  
  
  #--------------------------------#
  # regularize no-standard ages    #
  #--------------------------------# 
  if(age_in == "other") {
    
    value       <- data_in[, variable, drop = TRUE]
    age         <- data_in$Age
    value1       <- graduate_uniform(Value = value, 
                                     Age   = age)
    age1         <- names2age(value1)
    # If there is an infant group, we preserve it
    
    if(has_infants) {
      
      ageN       <- calcAgeAbr(age1)
      value      <- groupAges(Value = value1, 
                              Age   = age1, 
                              AgeN  = ageN)
      age        <- names2age(value)
      age_in     <- "abridged"
      
    } else {
    
      # otherwise, group to 5-year ages
      value      <- groupAges(Value = value1,
                              Age   = age1,
                              N     = 5)
      age_in     <- "5-year"
      age        <- names2age(value)
    }
    data_in <- tibble(Age = age,
                      !!variable := value)
  }
  
  # some helper objects for flexible outbound ages
  age1   <- min(age):max(age)
  ageN   <- switch(age_out,
                   "single"   = age1,
                   "5-year"   = age1 - age1 %% 5,
                   "abridged" = calcAgeAbr(age1))
  
  # -------------------------------------------------------#
  # simplest case, we do nothing to the age distribution,  #
  # but we *might* group data still...                     #
  # -------------------------------------------------------#
  if(rough_method == "none" & fine_method == "none") {
    
    if(age_out == age_in) {
      
      return(data_in)
      
    } 
    
    if(age_out == "single") {
      
      warning("You requested no fine or rough methods,\nbut you want single age output. We assumed a uniform distribution over single ages within the age groups given.")
      
    }
    
    value     <- data_in[, variable, drop = TRUE]
    age       <- data_in$Age
    value1    <- graduate_uniform(Value = value,
                                  Age   = age)
    age1      <- names2age(value1)
    ageN      <- switch(age_out,
                        "single"   = age1,
                        "5-year"   = age1 - age1 %% 5,
                        "abridged" = calcAgeAbr(age1))
    value_out <- groupAges(Value = value1,
                           Age   = age1,
                           AgeN  = ageN)
    data_out  <- tibble(Age = names2age(value_out),
                        !!variable := value_out)
    
    return(data_out)
    
  }
  
  # ------------------------#
  # I: Handle rough methods #
  # ------------------------#
  
  # this is a fallback data5
  data5 <- data_in |> 
    mutate(Age = Age - Age %% 5) |> 
    group_by(Age)  |> 
    summarize(!!variable := sum(!!sym(variable)))
  
  # (1) the case of auto everything (verify arguments to pass)
  if(rough_method == "auto") {
      
    data1 <- graduate_auto(data_in,
                           age_out  = "single",
                           variable = variable,
                           u5m      = u5m,
                           Sex      = Sex,
                           constrain_infants = constrain_infants)

    # regroup to 5, overrides previous one
    data5 <- data1 |> 
        mutate(Age = Age - Age %% 5) |> 
        group_by(Age) |> 
        summarize(!!variable := sum(!!sym(variable)))
      
  }
  
  # if the rough method was a specific one, we overwrite the value data5
  # smooth_age_5 does not have pclm option for methods argument REMOVED!
  if(rough_method %in% c("Carrier-Farrag", "KKN", "Arriaga",
                                 "United Nations", "Strong", "Zigzag")) {

    data5 <- data5 |>
      mutate(!!variable := smooth_age_5(Value  = !!sym(variable),
                                        Age    = Age,
                                        method = rough_method))
  
  }
   
  # NOTE: can't return 5-year output yet even if desired, because
  # some graduation methods shoft between 5-year age groups, and this
  # 'light' smoothing might be desired.
  
  if(fine_method == "none" & age_out == "5-year") {
    
    return(data5)
    
  }
  
  # -------------------------#
  # II: Handle fine methods  #
  # -------------------------#
  
  # I have an idea to keep the fine structure of our auto method while
  # constraining 5-year age groups to be whatever the above did. Odd,
  # I know, but maximally flexible? Here, we are presuming that (a) either
  # the data_in had no detectable 5-year heaping, or (b) any previously-selected
  # rough_method will have erased detectable smoothing. Otherwise, the auto method
  # will perturb at two levels, albeit not necessarily in the same way as if
  # data_in had both fine and rough methods as auto.
  if(fine_method == "none" & age_out %in% c("single","abridged")) {
    
    if(age_in == "single") {
      # this is odd indeed: under what circumstances would we want to adjust 
      # 5-year age groups but NOT single ages? In this case, we are strict,
      # and we preserve all proportions inside single ages
      
      data5 <- data5 |>
        rename(age5   = Age,
               value5 = !!sym(variable))
      
      data1 <- data_in |> 
        mutate(age5 = Age - Age %% 5) |> 
        mutate(prop = !!sym(variable) / sum(!!sym(variable)), .by = age5) |> 
        left_join(data5, by = join_by(age5)) |> 
        mutate(!!variable := !!sym(variable) * prop) |> 
        select(Age, !!sym(variable))
      
    }
      
    if(age_in != "single") {
        
      # TODO: also don't issue this warning if constrain_infants == TRUE
      # we can add that condition afterwards
      if (age_in == "5-year" & constrain_infants) {
        
        warning("We used graduate_mono() to split to single ages.\nThis (or another fine_method) was necessary because\nyou specified single-age output, but your input\ndoes not appear to be in single ages.")
        
      }
        
        fine_method <- "mono"
        # value  <- data_in |> 
        #             pull(!!sym(variable))
        # age    <- data_in |> 
        #             pull(Age)
        # value1 <- graduate_mono(Value = value,
        #                         Age = age,
        #                         OAG = TRUE)
        # age1   <- names2age(value1)
        # data1  <- tibble(Age = age1,
        #                 !!variable := value1)
    } 
    
  } 
  
  if(fine_method == "auto") {
    # Here we presume that data5 has no detectable sawtooth pattern,
    # meaning this was so in data_in or as the result of another 
    # rough_method having been applied. Otherwise, this will get picked
    # up in the auto method and taken care of with its MAV logic.
    if(rough_method != "auto") {
      
      data1 <- graduate_auto(data_in,
                             age_out  = "single",
                             variable = variable,
                             u5m      = u5m,
                             Sex      = Sex,
                             constrain_infants = constrain_infants)
    }
    
    data5 <- data5 |> 
      rename(age5   = Age,
             value5 = !!sym(variable) )
    
    data1 <- data1 |> 
      mutate(age5 = Age - Age %% 5) |> 
      mutate(prop = !!sym(variable) / sum(!!sym(variable)), .by = age5) |> 
      left_join(data5, by = join_by(age5)) |> 
      mutate(!!variable := !!sym(variable) * prop) |> 
      select(Age, !!sym(variable))
    
  }
  
  if(fine_method %in% c("sprague", "beers(ord)", "beers(mod)", 
                        "grabill", "pclm", "mono", "uniform")) {
    value  <- data5 |> 
                pull(!!sym(variable))
    age    <- data5 |> 
                pull(Age)
    value1 <- graduate(Value  = value,
                       Age    = age,
                       method = fine_method,
                       OAG    = TRUE)
    age1   <- names2age(value1)
    data1  <- tibble(Age = age1,
                    !!variable := value1)
    
  }
  # we got to single ages anyway, it's pragmatic, trust me
  
  #-----------------------------#
  # III group to desired output #
  #-----------------------------#
  
  value     <- data1 |> # was data1
    pull(!!sym(variable))
  age       <- data1 |>
    pull(Age)
  
  # causes problems too if rough_method = "auto", fine_method  = "none"
  # and age_out  = "abridged"
  value_out <- groupAges(Value = value,
                         Age   = age,
                         AgeN  = ageN)

  age      <- names2age(value_out)
  data_out <- tibble(Age = age,
                     !!variable := value_out)

  
  # TODO: here add the constraint_infants chunk.
  if(constrain_infants) { 
    
    varb <- data_in[, variable, drop = TRUE]
    Age  <- data_in$Age
    
    if(age_in == "5-year") { 
      
      if(age_out != "5-year") {
        
        if(!is.null(u5m)) {
          
          # in odd case that child mortality is given, but Sex is not specified:
          if(is.null(Sex)) {
            
            Sex <- "t"
            warning("Sex argument not given. We assumed total (Sex = 't'). We use this variable to inform splitting the infant age group.")
            
          }
          
          # we need this variable for indirect method applied
          stopifnot(Sex %in% c("f", "m", "t")) 
          
          if(variable == "Deaths") {
            
            D5 <- varb[1]
            P5 <- D5 / u5m
            
          }
          
          if(variable %in% c("Exp", "Exposures", "Pop", "Population")) {
            
            P5 <- varb[1]
            D5 <- P5 * u5m
            
          }
          
          if(Sex %in% c("f", "m")) {
            
            D0   <- lt_rule_4m0_D0(D04 = D5, 
                                   M04 = u5m, 
                                   Sex = Sex)
            
            M0   <- lt_rule_4m0_m0(D04 = D5, 
                                   M04 = u5m, 
                                   Sex = Sex)
            
            P0   <- D0 / M0
            
          } else {
            
            D0m  <- lt_rule_4m0_D0(D04 = D5, 
                                   M04 = u5m, 
                                   Sex = "m")
            
            D0f  <- lt_rule_4m0_D0(D04 = D5, 
                                   M04 = u5m, 
                                   Sex = "f")
            
            D0   <- (D0m + D0f) / 2
            
            M0m  <- lt_rule_4m0_m0(D04 = D5, 
                                   M04 = u5m, 
                                   Sex = "m")
            
            M0f  <- lt_rule_4m0_m0(D04 = D5, 
                                   M04 = u5m, 
                                   Sex = "f")
            
            M0   <- (M0m + M0f) / 2
            P0   <- D0 / M0
            
          }
          
          if(variable == "Deaths") {
            
            D1_4 <- D5 - D0
            varb <- c(D0, D1_4, varb[-1])
            Age  <- c(Age[1], 1, Age[-1])
            
          }
          
          if(variable %in% c("Exp", "Exposures", "Pop", "Population")) {
            P1_4 <- P5 - P0
            varb <- c(P0, P1_4, varb[-1])
            Age  <- c(Age[1], 1, Age[-1])
            
          }
          
          # For purposes of continued decision-making
          age_in       <- "abridged"
          fst_ages     <- varb[1:2]
          pct_fst_ages <- fst_ages / sum(fst_ages)
          
        } else {

            warning("Be mindful of results for the infant age group. Your output has a separate infant age group, but this was split from the input data without taking demographic knowledge into account. If you specify an under-5 mortality rate, u5m, we can do a better job.")

        }
        
      }
      
    }
    
    if(age_in == "abridged") {
      
      # calculate the distribution of first two ages for data_out if needed in future
      fst_ages     <- varb[1:2]
      pct_fst_ages <- fst_ages / sum(fst_ages)
      
      }
    
    if(age_in == "single") { 
      
      # calculate the distribution of first two ages for data_out if needed in future
      fst_ages     <- varb[1:5]
      fst_ages     <- c(fst_ages[1], sum(fst_ages[-1]))
      pct_fst_ages <- fst_ages / sum(fst_ages)
      
    }
    
    final_data_single <- is_single(data_out$Age)
    
    # (1) graduate_mono
    varb <- data_out[, variable, drop = TRUE]
    
    v2   <- graduate_mono(Value = varb, 
                          Age   = data_out$Age, 
                          OAG   = TRUE)
    
    age  <- as.integer(names(v2))
    
    # (2) regroup
    ageN <- switch(age_out,
                   "abridged" = calcAgeAbr(age),
                   "5-year"   = age - age %% 5,
                   "single"   = age)
    
    v3 <- groupAges(Value = v2, 
                    Age   = age, 
                    AgeN  = ageN)
    
    data_out <- tibble(!!variable := v3,
                       Age = as.integer(names(v3)))
    
    # (3) possibly constrain infants
    if(age_out %in% c("abridged", "single") & age_in %in% c("abridged", "single") & constrain_infants) {
      
      v_child      <- data_out[data_out$Age < 5, variable, drop = TRUE]
      vN           <- sum(v_child)
      v_child[1]   <- pct_fst_ages[1] * vN
      v_child[-1]  <- (1 - pct_fst_ages[1]) * vN * v_child[-1] / sum(v_child[-1])
      data_out[data_out$Age < 5, variable] <- v_child
    }
  
  }
  
  return(data_out)
  
}


# In my opinion there is no need for double list output and this will look waaay better
# So I will just keep it in here just in case
# plot_smooth_compare <- function(data_in, data_out, variable) {
#   
#   ggplot() +
#     geom_line( data = data_in,  aes(x = Age, y = !!sym(variable)), color = "black") +
#     geom_point(data = data_in,  aes(x = Age, y = !!sym(variable)), color = "black") +
#     geom_line( data = data_out, aes(x = Age, y = !!sym(variable)), color = "red", linewidth = 1) +
#     scale_x_continuous(breaks = pretty_breaks()) +
#     scale_y_continuous(breaks = pretty_breaks(), labels = scales::comma) +
#     theme_light() +
#     theme(axis.text = element_text(color = "black"),
#           plot.title = element_text(size = 12)) +
#     ggtitle(label = "Black connected dots correspond to original input data,\nwhile red line corresponds to the smoothed data")
#   
# }



#' @title plot_smooth_compare compares the original and smoothed counts over the chosen age ranges
#' @description Plots the original counts and the smoothed counts over the chocosen age ranges for ease of comparison.
#' @param data_in tibble. A tibble with two numeric columns - population or death counts - provided in single age intervals, 5-year age intervals, or abridged age format e.g. with ages 0, 1-4, 5-9 etc. 
#' @param data_out A tibble with two numeric columns - smoothed counts for the chosen variable and `Age` - chosen age grouping. The output from the `smooth_flexible` function.
#' @param variable character. A scalar with the variable name which is to be graduated. The list of possible options include `Pop`, `Population`, `Exp`, `Exposures` or `Deaths`.
#' @importFrom dplyr case_when mutate group_by summarize rename left_join select join_by pull
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_y_continuous scale_x_continuous theme_light theme element_text ggtitle
#' @importFrom scales pretty_breaks comma
#' @importFrom rlang := !! sym
#' @return list. A named list with 2 elements: `before` - plot of original input data and `after` - plot of smoothed data
#' #' @examples
#' This is just test settings used for light live coding.
#' data(pop1m_ind, package = "DemoTools")
#' data_in <- data.frame(Exposures = pop1m_ind,
#'                       Age = 0:100)
#'                       
#' data_out <- smooth_flexible(
#'                data_in, 
#'                variable     = "Exposures", 
#'                rough_method = "auto",
#'                fine_method  ="pclm", 
#'                constrain_infants = TRUE, 
#'                age_out = "5-year", 
#'                u5m     = .1,
#'                Sex     = "t")
#'                
#'plot_smooth_compare(data_in, data_out, variable = "Exposures")
plot_smooth_compare <- function(data_in, data_out, variable) {
  
  pt1 <- ggplot() +
    geom_line( data = data_in,  aes(x = Age, y = !!sym(variable)), color = "black") +
    geom_point(data = data_in,  aes(x = Age, y = !!sym(variable)), color = "black") +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(breaks = pretty_breaks(), labels = comma) +
    theme_light() +
    theme(axis.text = element_text(color = "black"),
          plot.title = element_text(size = 12)) +
    ggtitle(label = "Original input data")
  
  pt2 <- ggplot() +
    geom_line(data = data_out, aes(x = Age, y = !!sym(variable)), color = "red", linewidth = 1) +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(breaks = pretty_breaks(), labels = comma) +
    theme_light() +
    theme(axis.text = element_text(color = "black"),
          plot.title = element_text(size = 12)) +
    ggtitle(label = "Smoothed output data")
  
  return(list(before = pt1, 
              after  = pt2))
  
}