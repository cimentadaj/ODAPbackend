#' @title graduate_auto
#' @description Smooths the counts using the moving averages. The methods are adoзted from the "Method protocol for the evaluation of census population data by age and sex"
#' @param data_in tibble. A tibble with two columns - `Pop` - population counts and `Age` provided in single age intervals, 5 year age intervals or abridged format e.g. with ages 0, 1-4, 5-9 etc
#' @param variable character. A scalar with the variable name which is to be graduated. For example `Pop` or `Death`
#' @param age_out character. A scalar with the desired age grouping output. 3 possible options - `single` for single ages, `5-year` - for 5-year age groups, and `abridged` - for abridged data. NOTE, `abridged` will not work if the data_in is in 5-year groups, since we do not know the initial distributions in the abridged ages.
#' @return A named vector with graduated and smoothed population counts
#' @importFrom dplyr case_when mutate group_by filter pull
#' @importFrom tibble tibble
#' @importFrom DemoTools is_single check_heaping_bachi groupAges ageRatioScore mav graduate_mono
#' @return data_out. A tibble with two numeric columns - smoothed counts for the chosen variable and `Age` - chosen age grouping
#' @export 
#' @examples
# TR: why \dontrun{}?
#' \dontrun{ 
#' data_in <- data.frame(Pop = pop1m_ind,
#' Age = 0:100,
#' Pop = pop1m_ind)
#' 
#' graduate_auto(data_in = data_in,
#' age_out = "single", 
#' variable = "Pop"
#' )
#' }
# ---------------------------------------------------------------------- #

# 3 types of test data
# single years 
# data_in <- tibble(Pop = pop1m_ind,
#                   Age = 0:100,
#                   Death = pop1m_ind,
#                   Exposures = pop1m_ind)
 
# 5-years
# data_in <- tibble(Pop = groupAges(pop1m_ind, N = 5),
#                   Age = seq(0, 100, 5))
 
# abridged
# data_in <- tibble(Pop = c(groupAges(pop1m_ind, N = 5)[1] * c(0.4, 0.6),
#                           groupAges(pop1m_ind, N = 5)[-1]),
#                   Age = c(0, 1, seq(5, 100, 5)))

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

# tests
# graduate_auto(data_in,
#               age_out = "abridged", 
#               variable = "Pop")
# graduate_auto(data_in,
#               age_out = "5-year", 
#               variable = "Pop")
# graduate_auto(data_in,
#               age_out = "single",
#               variable = "Pop")

graduate_auto <- function(data_in, 
                          age_out = "single", 
                          variable) {

  # data and Age as vectors
  varb <- data_in[, variable, drop = TRUE]
  Age  <- data_in$Age
  
  # check if data comes in single
  sngl <- is_single(data_in$Age)
  
  # check if data is abridged
  abrgd <- is_abridged(data_in$Age)
  
  # TR: maybe we just make a 3-category checker for this task? Because
  # we need "single", "5-year", and "abridged" incoming options AND
  # outgoing options. would check_heaping_bachi() do the right thing 
  # for incoming abridged data? Maybe it needs to be 5-year grouped first?
  # Done. Just in case I will first transform abridged data and then calculate bachi
  # Bachi works for single years of age, so we need to graduate 5-year counts before calculating bachi
  
  # if data is abridged group first two ages. Next we can just use 5-year data protocol
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
      summarise(!!variable := sum(!!sym(variable)), .groups = "drop")
    
  }
  
  if(sngl) { 
    
    # data and Age as vectors
    varb <- data_in[, variable, drop = TRUE]
    Age  <- data_in$Age
    
    # calculate the distribution of first two ages for data_out if needed in future
    fst_ages     <- varb[1:5]
    fst_ages     <- c(fst_ages[1], sum(fst_ages[-1]))
    pct_fst_ages <- fst_ages / sum(fst_ages)
    
  }
  
  # if data is not in single ages, gradute_mono to single ages for bachi calculation
  # TR: is this needed? Do we calculate bachi for 5-year data? If bachi is 
  # being calculated on smooth-graduated data does it have meaning?
  if(!sngl) {
    
    # RTZ: graduate(varb, Age, method = "pclm") ???
    # TR: I like pclm more, but only when using offsets. Ideally,
    # we'd have an option to graduate death counts using population offsets,
    # thereby generating smooth mortality rates. But we can leave it for next
    # steps
    ungrpd_data <- graduate_mono(varb, Age, OAG = TRUE) 
    
    data_in_orig <- data_in
    
    data_in <- tibble(!!variable := ungrpd_data,
                      Age = 1:length(ungrpd_data) - 1)
    
    # data and Age as vectors
    varb <- data_in[, variable, drop = TRUE]
    Age  <- data_in$Age
    
  } else { 
    
    # data and Age as vectors
    varb <- data_in[, variable, drop = TRUE]
    Age  <- data_in$Age
    
    }
  
  # now we calculate adult bachi index
  bachi <- check_heaping_bachi(
    varb,
    Age     = Age,
    ageMin  = 23,
    ageMax  = 77, # following their example, not explicitly stated
    method  = "pasex",
    details = TRUE
  )
  
  # page 35 kids bachi index
  bachi_kids <- check_heaping_bachi(
    varb,
    Age     = Age,
    ageMin  = 3,
    ageMax  = 17, # following their example, not explicitly stated
    method  = "pasex",
    details = TRUE
  )
  
  # some indexes that we will use in the analysis
  # overall bachi index adults and kids
  index      <- bachi$index
  index_kids <- bachi_kids$index
  # pct for every number
  pct      <- bachi$pct
  pct_kids <- bachi_kids$pct
  
  # BachiProp0and5
  # Proportion of heaping concentrated in digits 0 and 5 adults and kids
  prp0and5     <- (sum(pct[c(1, 6)]) - 20)      / index
  prp0and5kids <- (sum(pct_kids[c(1, 6)]) - 20) / index_kids
  
  # Max2prop
  # Proportion of heaping concentrated in the most prefered 2 digits adults and kids
  mxprop2      <- (sum(sort(pct, decreasing = TRUE)[c(1, 2)]) - 20)      / index
  mxprop2_kids <- (sum(sort(pct_kids, decreasing = TRUE)[c(1, 2)]) - 20) / index_kids
  
  # 3 different conditions are considered
  # 1) if ages are single and bachi is more than or equal to 30
  # is this case group data into 5 year ages and apply 5-year protocol
  # 2) if ages single and index < 30, then we can use the protocol for single ages
  # 3) if the data is grouped already, then use protocol for grouped data
  
  # case 1 - ages is single
  if(sngl) {
    
    # if adult bachi more than 30
    if(index >= 30) { 
      
      # group data in 5 years
      # TR: this could be Pop OR Deaths, and note elsewhere in package
      # we have Exposures (or something like that) rather than Pop. So maybe
      # the clean option is to let the user specify which column we operate on?
      # Done. Great comment. Thank you. 
      
      cmbn_5_yrs <- groupAges(varb, N = 5)
      
      # TR: can we please use base R pipes unless otherwise necessary?
      # Done.
      dat_5 <- tibble(!!variable := cmbn_5_yrs,
                      Age = as.numeric(names(cmbn_5_yrs)))
      
      # apply protocol for 5 year data
      data_out <- graduate_auto_5(dat_5, variable = variable)
      
    } else { 
      
      # We don't have years of education. So I always choose the maximum mav of the two available
      
      # calculate n
      # n <- tibble(prp0and5,
      #             index,
      #             mxprop2) |>
      # TR: Why did you not use the table we had?
      # I would like this info as a table, for maintenance reasons.
      # easier to intervene on a table than tinker with this statement,
      # make sense? I don't care if the table is defined in situ, or whether
      # it's a csv in inst/extdata or an .rda in /data and documented in data.R
      # but please make this decision tree a tabular object rather than hard
      # coded as here.
      # Done. Replaced with the table you have previously approved.
      #   mutate(
      #     n = case_when(
      #       index >= 4    & index < 8    & prp0and5 >  0.65 ~ 10,
      #       index >= 4    & index < 8    & prp0and5 <= 0.65 ~ 6,
      #       index >= 2    & index < 4    & prp0and5 >  0.60 ~ 6,
      #       index >= 2    & index < 4    & prp0and5 <= 0.60 ~ 4,
      #       index >= 0.75 & index < 2    & mxprop2  >  0.70 ~ 4,
      #       index >= 0.75 & index < 2    & mxprop2  <= 0.70 ~ 2,
      #       index >= 0    & index < 0.75 & mxprop2  >  0.55 ~ 2,
      #       index >= 0    & index < 0.75 & mxprop2  <= 0.55 ~ 1,
      #       index >= 8    & index < 30                      ~ 10,
      #       TRUE                                            ~ NA
      #     )
      #   ) |>
      #   pull(n)
      
      n <- tibble(
        min_bachi    = c(4, 2, 0.75, 0,    8),
        max_bachi    = c(8, 4, 2,    0.75, 30),
        second_index = c(0.65, 0.6, 0.7, 0.55,101),
        ind          = c(rep(prp0and5, 2), rep(mxprop2, 2), Inf),
        mav_val_y    = c(10, 6, 4, 2, 10),
        mav_val_n    = c(6,  4, 2, 1, 10)) |>
        filter(min_bachi < index & max_bachi >= index) |>
        mutate(my_n = if_else(ind > second_index, mav_val_y, mav_val_n)) |>
        pull(my_n)
      
      # smooth accordingly
      # NOTE: in methods on pictures I do not see the division into adults and kids,for single year data
      # but in the text they mention it, so maybe
      # Not Done. I have many questions regarding this part. Nothing is clear for me here
      
      data_out <-  mav(
        Value = data_in[, variable, drop = TRUE],
        Age   = data_in$Age,
        n     = n,
        tails = TRUE
      )
      
      }
    
  } else { 
    
    # case 3, if the data is in 5 year age groups
    # NOTE: maybe remove this part
    # This is my attempt to catch the abridged data e.g ages 0 and 1-4 and separate it from 0-4
    # I assume that the ratio of population at first and second age will exceed 2.6 only if the first age is 0 and second age is grouped 1-4.
    # Otherwise if we have ages grouped as 0-4 and 5-9 it should not exceed 2.6.
    # Maybe better ways of checking this? Save for simply checking if ages are given as 0, 1-4 or not?
    
    # TR: you just want is_abridged()? This you judge from Age column rather
    # than a value column. The ratio relationship would be different 
    # anyway depending which value is checked...
    # check_abridged <- (dat$Pop[1] / dat$Pop[2]) > 2.6
    # 
    # if (check_abridged) {
    #   dat <- dat |>
    #     mutate(Age = c(0, 0, (.$Age)[-c(1:2)])) |>
    #     group_by(Age) |>
    #     summarise(Pop = sum(Pop), .groups = "drop")
    #   
    # }
    # DONE, fixed and moved upward.
    
    # If the data is already grouped and not abridged, then apply the 5 year method directly
    data_out <- graduate_auto_5(dat_5 = data_in_orig, variable)
    
    }
  
  # TR: here provide the final grouping operation to abide by age_out specification
  # "abridged", "single", "5-year". graduate_auto_5() will spit back 5-year ages.
  # Done. This is actually complex task. Because our data can be in 2 types and the uput iis 3 types 
  # so it is 6 combinations 
  final_data_single <- is_single(data_out$Age)
  
  if(age_out == "single" & final_data_single) { 
    
    data_out <- data_out
    
  }
  
  if(age_out == "single" & !final_data_single) { 
  
    data_var <- graduate_mono(data_out[ , variable, drop = TRUE], data_out$Age, OAG = TRUE)
  
    data_out <- tibble(!!variable := data_var,
                       Age = names(data_var))    
    
  }
  
  if(age_out == "5-year" & !final_data_single) { 
    
    data_out <- data_out
    
  }
  
  if(age_out == "5-year" & final_data_single) { 
    
    
    data_var <- groupAges(data_out[, variable, drop = TRUE], N = 5)
    
    data_out <- tibble(!!variable := data_var,
                       Age = names(data_var))    
    
  }
  
  if(age_out == "abridged" & !final_data_single) {
    
    data_var <- c(data_out[1, variable, drop = TRUE] * pct_fst_ages, data_out[-1, variable, drop = TRUE])
    
    data_out <- tibble(!!variable := data_var,
                       Age = c(0, 1, as.numeric(names(data_var)[-c(1, 2)])))
    
    }
  
  if(age_out == "abridged" & final_data_single) { 
    
    data_var <- groupAges(data_out[, variable, drop = TRUE], N = 5)
    
    data_var <- c(data_out[1, variable, drop = TRUE] * pct_fst_ages, data_out[-1, variable, drop = TRUE])
    
    data_out <- tibble(!!variable := data_var,
                       Age = c(0, 1, as.numeric(names(data_var)[-c(1, 2)])))
    }
  
  return(data_out)
  
}
#' @title graduate_auto_5
#' @description Implements the method protocol procedure for data with 5-year age groups
#' @param dat_5 tibble. A tibble with two columns - `Pop` - population counts in 5-year age groups and corresponding `Age` column (lower bound of age group)
#' @param variable character. A scalar with the variable name which is to be graduated. For example `Pop` or `Death`
#' @return A named vector with graduated and smoothed population counts
#' @importFrom dplyr mutate filter pull case_when
#' @importFrom tibble tibble
#' @importFrom DemoTools mav graduate_mono ageRatioScore
#' @return data_out. A tibble with two numeric columns - smoothed counts for the chosen variable and `Age` - chosen age grouping
#' @export 
#' 
# TR: add a logical argument called constrain_infant_proportion
# if constrain_infant_proportion TRUE then if abridged data are incoming,
# we apply the procedure below, then split 0-4 into 0, 1-4 using the original
# proportion, i.e. abridged data outgoing. This can happen either in this
# function or outside of it in the above function.
# Done. This issue is being addressed in the upper level function, so no need to add arg. here.
# or we can use smooth_age_5 ??? page 31 par. 1
# maximum of two sexes is used
# first graduate data by 5-year of age, and then check for bachi and combine them again?
# child - page 36. 
graduate_auto_5 <- function(dat_5, variable) {
  
  # separate data into kids and adults
  # figures say age 0-14, but this is only 3 ages, it will not work
  # then in text they say calculate age ratio for ages 0-24 AND then smooth only ages 0-19
  # I use the second option but there is a clear contradiction in text and 
  # I`m not sure it is exactly right
  # TR: OK, we leave this note here and can ask about it in future.
  
  kids <- dat_5 |>
    filter(Age < 20)
  
  adults <- dat_5 |> # ages 15 to Inf
    filter(Age > 10)
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
                             Age = as.numeric(names(dat5_mav_adults))) |>
    filter(Age < 75)
  
  age_rat_score_adults_2 <-
    ageRatioScore(
      Value  = age_rat_adults_2[, variable, drop = TRUE],
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
  # Values are exactly the same, I do not understand why I need to blend them
  # So I just pick up one
  data_full <- c(data_kids[-length(data_kids)], data_adults)
  
  # dt_fn <- structure(data_full,
  #                    .Names = names(data_full))
  # 
  # TR: this graduation shouldn't happen in here? Just spit back 5-year ages?
  # Done. Sure, as you say. From the report I had an impression that they ALWAYS want single data back
  # graduate to single ages
  # data_out <- graduate_mono(dt_fn, OAG = TRUE)
  
  # if we want transform back to ages 0, 1-4, we proportionally scale the age 0-5
  # DONE This will happen in upper function
  # if(constrain_infant_proportion) { 
  #   
  #   data_full <- c(data_full[1] * pct_fst_ages, data_full[-1])
  #   
  #   data_out <- tibble(!!variable := data_full,
  #                      Age = c(1, 4, as.numeric(names(data_full)[-1])))
  # 
  #   
  # } else { # otherwise return as is
  #   
  #   data_out <- tibble(!!variable := data_full,
  #                      Age = as.numeric(names(data_full)))
  #   
  #   }

  data_out <- tibble(!!variable := data_full,
                     Age = as.numeric(names(data_full)))
  
  return(data_out)
  
}
