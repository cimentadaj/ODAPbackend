#' @title graduate_auto
#' @description Smooth population of death counts with moving averages. The method was adopted from the "Method protocol for the evaluation of census population data by age and sex" paragraph 5.
#' @param data_in tibble. A tibble with two columns - `Pop` or any other column with population or death counts and corresponding `Age` -  provided in single age intervals, 5 year age intervals or abridged format e.g. with ages 0, 1-4, 5-9 etc.
#' @param variable character. A scalar with the variable name which is to be graduated. For example `Pop` or `Death`, or `Exposures`
#' @param age_out character. A scalar with the desired age grouping output. Includes 3 possible options - `single` for single ages, `5-year` - for 5-year age groups, and `abridged` - for abridged data. NOTE, `abridged` will not work if the data_in is in 5-year groups, since we do not know the initial distributions in the abridged ages. This behaviour will be adjusted in future versions.
#' @param constrain_infants logical. A scalar indicating weather the infant proportions should be constrained or left as is.
#' @return A tibble with 2 columns - your chosen `variable` with graduated and smoothed counts and `Age`
#' @importFrom dplyr mutate group_by filter pull select summarise
#' @importFrom tibble tibble
#' @importFrom rlang := !!
#' @importFrom DemoTools is_single check_heaping_bachi groupAges ageRatioScore mav graduate_mono calcAgeAbr
#' @return data_out. A tibble with two numeric columns - smoothed counts for the chosen variable and `Age` - chosen age grouping
#' @export
#' @examples
#' data(pop1m_ind, package = "DemoTools")
#' data_in <- data.frame(Pop = pop1m_ind,
#' Age = 0:100,
#' Pop = pop1m_ind)
#' 
#' ex1 <- graduate_auto(
#' data_in  = data_in,
#' age_out  = "single", 
#' variable = "Pop",
#' constrain_infants = TRUE)
#'
#' ex2 <- graduate_auto(
#' data_in  = data_in,
#' age_out  = "single", 
#' variable = "Pop",
#' constrain_infants = FALSE)
#'
#' ex3 <- graduate_auto(
#' data_in = data_in,
#' age_out = "5-year", 
#' variable = "Pop",
#' constrain_infants = TRUE)
#'
#' ex4 <- graduate_auto(
#' data_in  = data_in,
#' age_out  = "abridged", 
#' variable = "Pop",
#' constrain_infants = FALSE)

# ---------------------------------------------------------------------- #
# 3 types of test data
# single years 
# data_in <- tibble(Pop = pop1m_ind,
#                   Age = 0:100,
#                   Death = pop1m_ind,
#                   Exposures = pop1m_ind)

# # 5-years
# data_in <- tibble(Pop = groupAges(pop1m_ind, N = 5),
#                   Age = seq(0, 100, 5))
# 
# # abridged
# data_in <- tibble(Pop = c(groupAges(pop1m_ind, N = 5)[1] * c(0.4, 0.6),
#                           groupAges(pop1m_ind, N = 5)[-1]),
#                   Age = c(0, 1, seq(5, 100, 5)))
# 
# # Are these the same?
# graduate_auto(data_in,
#               age_out = "single",
#               variable = "Exposures",
#               constrain_infants = TRUE)
# 
# graduate_auto(data_in,
#               age_out = "single",
#               variable = "Exposures",
#               constrain_infants = FALSE)
# 
# # good
# graduate_auto(data_in,
#               age_out = "5-year",
#               variable = "Exposures",
#               constrain_infants = TRUE)
# 
# 
# graduate_auto(data_in,
#               age_out = "5-year",
#               variable = "Exposures",
#               constrain_infants = FALSE)
# 
# # good
# graduate_auto(data_in,
#               age_out = "abridged",
#               variable = "Exposures",
#               constrain_infants = TRUE)
# 
# 
# graduate_auto(data_in,
#               age_out = "abridged",
#               variable = "Exposures",
#               constrain_infants = FALSE)

# Note: abridged age_out not working with 5-year data, since the proportions are unknown
# what to do with single ages kids? I do not see clear instructions
# Just apply the same algorithm but for recalculated indicators like bachi and proportions?
# NOTE this will make function exponentially more complex

# TR: You'll need to explain this. Note, DemoTools has lt_rule_4m0_D0(), which seems doable
# in our case, with just one more function arg. That would handle deaths, whereas 
# use together with lt_rule_4m0_m0() could help back out Population for infants. This would
# require deaths and exposures being mutually aware when handled, but this should
# be OK if both are available in data_in, which we currently guarantee. So... We
# have all info required to split 0-4 into infants and 1-4.
# RT: Got it. As was discused currently is left as is, and we will adjust it in future versions.
graduate_auto <- function(data_in, 
                          age_out  = NULL, 
                          variable = NULL,
                          constrain_infants = TRUE) {
  
  # check if data comes in single ages
  sngl <- is_single(data_in$Age)
  
  # check if data is in abridged ages
  abrgd <- is_abridged(data_in$Age)
  
  # if data is abridged, then group first two ages. Next we can just use 5-year data protocol.
  # Also calculate the proportion in first ages in case we want to retutn the abridged data.
  if(abrgd) { 
    
    # data and Age as vectors
    varb <- data_in[, variable, drop = TRUE]
    Age  <- data_in$Age
    
    # calculate the distribution of first two ages for data_out if needed in future
    fst_ages     <- varb[1:2]
    pct_fst_ages <- fst_ages / sum(fst_ages)
    
    # group first two ages into one. uses non-standard evaluation
    data_in <- data_in |>
      mutate(Age = c(0, 0, Age[-c(1:2)])) |>
      group_by(Age) |>
      summarise(!!variable := sum(!!sym(variable)), .groups = "drop") |>
      select(!!variable, Age)
    
  }
  
  # if single, then save variables and calculate the proportion in first ages in case age_out is abridged
  if(sngl) { 

    # data and Age as vectors
    varb         <- data_in[, variable, drop = TRUE]
    Age          <- data_in$Age
    
    # calculate the distribution of first two ages for data_out if needed in future
    fst_ages     <- varb[1:5]
    fst_ages     <- c(fst_ages[1], sum(fst_ages[-1]))
    pct_fst_ages <- fst_ages / sum(fst_ages)
    
    # now we calculate adult bachi index. NOTE we need this only in case of the single year data.
    # Otherwise we jump to 5-year protocol
    bachi <- check_heaping_bachi(
      varb,
      Age     = Age,
      ageMin  = 23, # same, we can play with this if we want
      ageMax  = 77, # following their example, not explicitly stated
      method  = "pasex",
      details = TRUE
    )
    
    # some indexes that we will use in the analysis. Only valid for single age data
    # overall bachi index adults
    index <- bachi$index
    
    # pct for every number
    pct <- bachi$pct
    
    # BachiProp0and5
    # Proportion of heaping concentrated in digits 0 and 5 adults
    prp0and5 <- (sum(pct[c(1, 6)]) - 20) / index
    
    # Max2prop
    # Proportion of heaping concentrated in the most preferred 2 digits adults
    mxprop2 <- (sum(sort(pct, decreasing = TRUE)[c(1, 2)]) - 20) / index
    
  }
  
  # if data is in 5-years just save the variables and do nothing else for now it.
  if(!sngl & !abrgd) { 
    
    varb <- data_in[, variable, drop = TRUE]
    Age  <- data_in$Age
    
    }
  
  # # if data is not in single ages, gradute_mono to single ages for bachi calculation
  # TR: is this needed? Do we calculate bachi for 5-year data? If bachi is 
  # being calculated on smooth-graduated data does it have meaning? Remove this step,
  # RT: DONE
  # # jump to 5-year protocol if data are abridged or in 5-year age groups
  # if(!sngl) {
  #   
  #   # RTZ: graduate(varb, Age, method = "pclm") ???
  #   # TR: I like pclm more, but only when using offsets. Ideally,
  #   # we'd have an option to graduate death counts using population offsets,
  #   # thereby generating smooth mortality rates. But we can leave it for next
  #   # steps
  #   ungrpd_data <- graduate_mono(varb, Age, OAG = TRUE) 
  #   
  #   data_in_orig <- data_in
  #   
  #   data_in <- tibble(!!variable := ungrpd_data,
  #                     Age = 1:length(ungrpd_data) - 1)
  #   
  #   # data and Age as vectors
  #   varb <- data_in[, variable, drop = TRUE]
  #   Age  <- data_in$Age
  #   
  # } else { 
  #   # TR: this else covers the case of ____
  #   
  #   # data and Age as vectors
  #   varb <- data_in[, variable, drop = TRUE]
  #   Age  <- data_in$Age
  #   
  #   }
  
  # 3 different conditions are considered
  # 1) if ages are single and bachi is more than or equal to 30
  # is this case group data into 5 year ages and apply 5-year protocol
  # 2) if ages single and index < 30, then we can use the protocol for single ages
  # 3) if the data is grouped already, then use protocol for grouped data
  
  # case 1 - ages is single
  if(sngl) {
    
    # if adult bachi more than 30, then we have o problem. We just apply the 5-year protol that works
    if(index >= 30) { 
      
      # group data in 5 years
      cmbn_5_yrs <- groupAges(Value = varb, 
                              Age   = Age, 
                              N     = 5)
      
      dat_5 <- tibble(!!variable := cmbn_5_yrs,
                      Age = as.integer(names(cmbn_5_yrs)))
      
      # apply protocol for 5 year data
      data_out <- graduate_auto_5(dat_5, variable = variable)
      
    } else { 
      # Single year protocol is implemented if bachi is less than 30
      # NOTE: we separate kids and adults for now. We follow the protocol in case of adults adults
      # For kids, I save them separately, and since it is currently unclear what to do, I do the following:
      # IF the adult n for smoothing is less than 3, then I keep kids as is (n_kids = 1)
      # ELSE if it is more than 2, then I apply smoothing with n = 2 for consistency.
      # Otherwise we might have a very strange final distribution with erratic kids pattern.
      # Imagine adults being smoothed with n = 10, while kids left as is.
      # Why I use 2? Well simply because for 5-year data they say that max n for kids should be equal to 2. 
      # So I simply adopted it from there. We might change this in future, if we will not figure a better way of smoothing for kids.
      
      # We don't have years of education. 
      # So I choose the maximum n for mav from the two available in the table.
      n <- tibble(
        min_bachi    = c(4, 2, 0.75, 0,    8),
        max_bachi    = c(8, 4, 2,    0.75, 30),
        second_index = c(0.65, 0.6, 0.7, 0.55,101),
        ind          = c(rep(prp0and5, 2), rep(mxprop2, 2), Inf),
        mav_val_y    = c(10, 6, 4, 2, 10),
        mav_val_n    = c(6,  4, 2, 1, 10)) |>
        filter(min_bachi < index & max_bachi >= index) |>
        mutate(my_n = ifelse(ind > second_index, mav_val_y, mav_val_n)) |>
        pull(my_n)
      
      # n for kids
      n_kids <- ifelse(n < 3, 1, 2)
      
      # First separate kids and adults
      
      # kids
      kids <- data_in |>
        filter(Age < 18) |>
        pull(variable)
      
      # adults
      adults <- data_in |>
        filter(Age > 17) |>
        pull(variable)
      
      # smoothing adults
      data_ad <- mav(
        Value = adults,
        Age   = Age[Age > 17],
        n     = n,
        tails = TRUE
      )
      
      # smoothing kids
      data_kd <- mav(
        Value = kids,
        Age   = Age[Age < 18],
        n     = n_kids,
        tails = TRUE
      )
      
      # combine adults and kids
      data_out <- tibble(!!variable := c(data_kd, data_ad),
                        Age = as.integer(names(c(data_kd, data_ad))))
      
      }
    
  } else { 
    
    # case 3, if the data is in 5 year age groups
    # If the data is already grouped and not abridged, then apply the 5 year method directly
    data_out <- graduate_auto_5(dat_5 = data_in, variable)
    
    }
  
  # TR: here provide the final grouping operation to abide by age_out specification
  # "abridged", "single", "5-year". graduate_auto_5() will spit back 5-year ages.
  # Done. This is actually complex task. Because our data can be in 2 types and the output is 3 types 
  # so it is 6 combinations 
  # TR: Actually it's easy: 
  # step 1: move to single, no matter what. 
  # step 2: group to desired output.
  # RT: Done.
  final_data_single <- is_single(data_out$Age)
  
  # (1) graduate_mono
  varb <- data_out[,variable, drop = TRUE]
  
  v2   <- graduate_mono(Value = varb, 
                        Age   = data_out$Age, 
                        OAG   = TRUE)
  
  age  <- as.integer(names(v2))
  
  # (2) regroup
  if(age_out == "abridged") {
    
    ageN <-  calcAgeAbr(age)
    
  }
  
  if(age_out == "5-year") {
    
    ageN <- age - age %% 5
    
  }
  
  if(age_out == "single") {
    
    ageN <- age
    
  }
  
  v3 <- groupAges(Value = v2, 
                  Age   = age, 
                  AgeN  = ageN)
  
  data_out <- tibble(!!variable := v3,
                     Age = as.integer(names(v3)))
  
  # (3) possibly constrain infants
  if (age_out %in% c("abridged", "single") & (sngl | abrgd) & constrain_infants) {

    v_child      <- data_out[data_out$Age < 5, variable, drop = TRUE]
    vN           <- sum(v_child)
    v_child[1]   <- pct_fst_ages[1] * vN
    v_child[-1]  <- (1 - pct_fst_ages[1]) * vN * v_child[-1] / sum(v_child[-1])
    data_out[data_out$Age < 5, variable] <- v_child

  }
  # pct_fst_ages <- fst_ages / sum(fst_ages)
  
  # possibly constrain infants if needed.
  
  # if(age_out == "single" & final_data_single) { 
  #   
  #   data_out <- data_out
  #   
  # }
  # 
  # if(age_out == "single" & !final_data_single) { 
  # 
  #   data_var <- graduate_mono(data_out[ , variable, drop = TRUE], data_out$Age, OAG = TRUE)
  # 
  #   data_out <- tibble(!!variable := data_var,
  #                      Age = names(data_var))    
  #   
  # }
  # 
  # if(age_out == "5-year" & !final_data_single) { 
  #   
  #   data_out <- data_out
  #   
  # }
  # 
  # if(age_out == "5-year" & final_data_single) { 
  #   
  #   
  #   data_var <- groupAges(data_out[, variable, drop = TRUE], N = 5)
  #   
  #   data_out <- tibble(!!variable := data_var,
  #                      Age = names(data_var))    
  #   
  # }
  # 
  # if(age_out == "abridged" & !final_data_single) {
  #   
  #   data_var <- c(data_out[1, variable, drop = TRUE] * pct_fst_ages, data_out[-1, variable, drop = TRUE])
  #   
  #   data_out <- tibble(!!variable := data_var,
  #                      Age = c(0, 1, as.numeric(names(data_var)[-c(1, 2)])))
  #   
  #   }
  # 
  # if(age_out == "abridged" & final_data_single) { 
  #   
  #   data_var <- groupAges(data_out[, variable, drop = TRUE], N = 5)
  #   
  #   data_var <- c(data_out[1, variable, drop = TRUE] * pct_fst_ages, data_out[-1, variable, drop = TRUE])
  #   
  #   data_out <- tibble(!!variable := data_var,
  #                      Age = c(0, 1, as.numeric(names(data_var)[-c(1, 2)])))
  #   }
  # 
  return(data_out)
  
  }

#' @title graduate_auto_5
#' @description Implements the method protocol procedure for data with 5-year age groups
#' @param dat_5 tibble. A tibble with two columns - `Pop` - population counts in 5-year age groups and corresponding `Age` column (lower bound of age group)
#' @param variable character. A scalar with the variable name which is to be graduated. For example `Pop` or `Death`
#' @return A tibble with 2 columns - your chosen `variable` with graduated and smoothed counts and `Age`
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom rlang := !!
#' @importFrom DemoTools mav graduate_mono ageRatioScore
#' @return data_out. A tibble with two numeric columns - smoothed counts for the chosen variable and `Age` - chosen age grouping
#' @export
graduate_auto_5 <- function(dat_5, variable) {
  
  # separate data into kids and adults
  # figures say age 0-14, but this is only 3 ages, it will not work
  # then in text they say calculate age ratio for ages 0-24 AND then smooth only ages 0-19
  # I use the second option but there is a clear contradiction in text and 
  # I`m not sure it is exactly right
  # TR: OK, we leave this note here and can ask about it in future.
  
  kids <- dat_5 |>
    filter(Age < 20)
  
  # ages 15 to Inf
  adults <- dat_5 |>
    filter(Age > 14)
  
  # calculate the age ratio score before smoothing separately for kids and adults
  # Only for age score, we add one additional age to kids
  rsc_kids <- dat_5 |>
    filter(Age < 24)
  
  age_rat_score_kids <- ageRatioScore(Value = rsc_kids[, variable, drop = TRUE],
                                      Age   = rsc_kids$Age) # check this
  
  # for adults use ages 15-19 : 70-74 for score calculation
  rsc_adults <- adults |>
    filter(Age < 75)
  
  age_rat_score_adults <- ageRatioScore(
    Value  = rsc_adults[, variable, drop = TRUE],
    Age    = rsc_adults$Age,
    ageMin = min(rsc_adults$Age)
  )
  
  # smooth data with mav = 2 for adults
  dat5_mav_adults <- mav(
    Value = adults[, variable, drop = TRUE],
    Age   = adults$Age,
    n     = 2,
    tails = TRUE
  )
  
  # calculate the age ratio score after smoothing for adults
  age_rat_adults_2 <- tibble(!!variable := dat5_mav_adults,
                             Age = as.integer(names(dat5_mav_adults))) |>
    filter(Age < 75)
  
  age_rat_score_adults_2 <-
    ageRatioScore(
      Value    = age_rat_adults_2[, variable, drop = TRUE],
      Age      = age_rat_adults_2$Age,
      ageMin   = min(age_rat_adults_2$Age)
    )
  
  # calculate the smoothing n for adults, new way
  n_choice_ad <- c(age_rat_score_adults < 4, age_rat_score_adults_2 < 4, age_rat_score_adults_2 >= 4)
  n           <- c(1, 2, 4)
  adult_n     <- n[n_choice_ad]

  # old way
  # adult_n <- tibble(unsm = age_rat_score_adults,
  #                   sm   = age_rat_score_adults_2) |>
  #   mutate(n = case_when(unsm < 4  ~ 1,
  #                        sm   < 4  ~ 2,
  #                        sm   >= 4 ~ 4)) |>
  #   pull(n)
  
  # calculate the smoothing n for kids, new way
  n_choice_kd <- c(age_rat_score_kids < 4, age_rat_score_kids >= 4)
  n           <- c(1, 2)
  kids_n      <- n[n_choice_kd]

  # old way  
  # kids_n <- tibble(unsm = age_rat_score_kids) |>
  #   mutate(n = ifelse(age_rat_score_kids < 4, 1, 2)) |>
  #   pull(n)
  
  # smooth kids
  data_kids   <- mav(
    Value = kids[, variable, drop = TRUE],
    Age   = kids$Age,
    n     = kids_n,
    tails = TRUE
  )
  
  # smooth adults
  data_adults <- mav(
    Value = adults[, variable, drop = TRUE],
    Age   = adults$Age,
    n     = adult_n,
    tails = TRUE
  )
  
  # combine NOTE redistribute with linear weight assumption
  # NOTE: for 5 year ages there is only 1 age group that blends 15-19
  # Age values are exactly the same, so we just keep one
  # Can be changed in future when we figure out what exactly are we supposed to do.
  data_full <- c(data_kids[-length(data_kids)], data_adults)
  
  data_out <- tibble(!!variable := data_full,
                     Age = as.integer(names(data_full)))
  
  return(data_out)
  
}