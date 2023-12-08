#' @title graduate_auto
#' @description Smooths the counts using the moving averages. The methods are adoзted from the "Method protocol for the evaluation of census population data by age and sex"
#' @param dat tibble. A tibble with two columns - `Pop` - population counts and `Age` provided in single age intervals, 5 year age intervals or abridged format e.g. with ages 0, 1-4, 5-9 etc
#' @return A named vector with graduated and smoothed population counts
#' @importFrom dplyr case_when mutate group_by filter pull
#' @importFrom tibble tibble
#' @importFrom DemoTools is_single check_heaping_bachi groupAges ageRatioScore mav graduate_mono
#' @export
#' @examples
#' \dontrun{
#' dat <- tibble(Pop = pop1m_ind,
#' Age = 0:100)
#'
#' graduate_auto(dat = dat)
#' }
#'
#'

# TR: this needs an age_out argument.
# incoming data can be called data_in
# outgoing data can be called data_out
graduate_auto <- function(dat) {
  # 1) check if data comes in single
  sngl <- is_single(dat$Age)
  
  # TR: maybe we just make a 3-category checker for this task? Because
  # we need "single", "5-year", and "abridged" incoming options AND
  # outgoing options. would check_heaping_bachi() do the right thing 
  # for incoming abridged data? Maybe it needs to be 5-year grouped first?
  
  # 2) calulate adult bachi
  bachi <- check_heaping_bachi(
    dat$Pop,
    Age     = dat$Age,
    ageMin  = 23,
    ageMax  = 77,
    method  = "pasex",
    details = TRUE
  )
  
  # some indexes that we will use in the analysis
  # index
  index <- bachi$index
  
  # pct for every number
  pct   <- bachi$pct
  
  # BachiProp0and5
  # Proportion of heaping concentrated in digits 0 and 5
  prp0and5 <- (sum(pct[c(1, 6)]) - 20) / index
  
  # Max2prop
  # Proportion of heping concentrated in the most prefered 2 digits
  mxprop2 <-
    (sum(sort(pct, decreasing = TRUE)[c(1, 2)]) - 20) / index
  
  # 3 different conditions are considered
  # 1) if ages are single and bachi is more than or equal to 30
  # is this case group data into 5 year ages and graduate accordingly
  # 2) if ages single and index < 30, then we use the protocol for single ages
  # 3) if the data is grouped already, then use protocol for grouped data
  
  # here is case 1 - ages is single and bachi >= 30
  if (sngl & index >= 30) {
    # group data in 5 years
    
    # TR: this could be Pop OR Deaths, and note elsewhere in package
    # we have Exposures (or something like that) rather than Pop. So maybe
    # the clean option is to let the user specify which column we operate on?
    cmbn_5_yrs <- groupAges(dat$Pop, N = 5)
    
    # TR: can we please use base R pipes unless otherwise necessary?
    dat_5 <- tibble(Pop = cmbn_5_yrs,
                    Age = names(cmbn_5_yrs)) %>%
      mutate(Age = as.numeric(Age))
    
    # apply protocol for 5 year data
    final <- graduate_auto_5(dat_5)
    
  }
  
  # case 2 - ages single but bachi is less than 30, use single ages protocol
  if (sngl & index < 30) {
    # We dont have years of education. So I always choose the maximum mav of the two available

    # calculate n
    n <- tibble(prp0and5,
                index,
                mxprop2) %>%
      # TR: Why did you not use the table we had?
      # I would like this info as a table, for maintenance reasons.
      # easier to intervene on a table than tinker with this statement,
      # make sense? I don't care if the table is defined in situ, or whether
      # it's a csv in inst/extdata or an .rda in /data and documented in data.R
      # but please make this decision tree a tabular object rather than hard
      # coded as here.
      mutate(
        n = case_when(
          index >= 4    & index < 8    & prp0and5 >  0.65 ~ 10,
          index >= 4    & index < 8    & prp0and5 <= 0.65 ~ 6,
          index >= 2    & index < 4    & prp0and5 >  0.60 ~ 6,
          index >= 2    & index < 4    & prp0and5 <= 0.60 ~ 4,
          index >= 0.75 & index < 2    & mxprop2  >  0.70 ~ 4,
          index >= 0.75 & index < 2    & mxprop2  <= 0.70 ~ 2,
          index >= 0    & index < 0.75 & mxprop2  >  0.55 ~ 2,
          index >= 0    & index < 0.75 & mxprop2  <= 0.55 ~ 1,
          index >= 8    & index < 30                      ~ 10,
          TRUE                                            ~ NA
        )
      ) %>%
      pull(n)
    
    # smooth accordingly
    # NOTE: in methods on pictures I do not see the division into adults and kids,for single year data
    # but in the text they mention it, so maybe it is needed here too. Not sure.
    # TR: yes, as far as I know, please divide
    final <- mav(
      Value = dat$Pop,
      Age   = dat$Age,
      n     = n,
      tails = TRUE
    )
    
  }
  
  # case 3, if the data is in 5 year age groups
  if (!sngl) {
    # NOTE: maybe remove this part
    # This is my attempt to catch the abridged data e.g ages 0 and 1-4 and separate it from 0-4
    # I assume that the ratio of population at first and second age will exceed 2.6 only if the first age is 0 and second age is grouped 1-4.
    # Otherwise if we have ages grouped as 0-4 and 5-9 it should not exceed 2.6.
    # Maybe better ways of checking this? Save for simply checking if ages are given as 0, 1-4 or not?
    
    # TR: you just want is_abridged()? This you judge from Age column rather
    # than a value column. The ratio relationship would be different 
    # anyway depending which value is checked...
    check_abridged <- (dat$Pop[1] / dat$Pop[2]) > 2.6
    
    if (check_abridged) {
      dat <- dat %>%
        mutate(Age = c(0, 0, (.$Age)[-c(1:2)])) %>%
        group_by(Age) %>%
        summarise(Pop = sum(Pop), .groups = "drop")
      
    }
    
    # If the data is already grouped and not abridged, then apply the 5 year method directly
    dat_5 <- dat %>%
      mutate(Age = as.numeric(Age))
    
    final <- graduate_auto_5(dat_5)
    
  }
  
  # TR: here provide the final grouping operation to abide by age_out specification
  # "abridged", "single", "5-year". graduate_auto_5() will spit back 5-year ages.
  
  return(final)
  
}
#' @title graduate_auto_5
#' @description Implements the method protocol procedure for data with 5-year age groups
#' @param dat_5 tibble. A tibble with two columns - `Pop` - population counts in 5-year age groups and corresponding `Age` column (lower bound of age group)
#' @return A named vector with graduated and smoothed population counts
#' @importFrom dplyr mutate filter pull case_when
#' @importFrom tibble tibble
#' @importFrom DemoTools mav graduate_mono ageRatioScore
#' @export

# TR: add a logical argument called constrain_infant_proportion
# if constrain_infant_proportion TRUE then if abridged data are incoming,
# we apply the procedure below, then split 0-4 into 0, 1-4 using the original
# proportion, i.e. abridged data outgoing. This can happen either in this
# function or outside of it in the above function.
graduate_auto_5 <- function(dat_5) {
  # separate data into kids and adults
  # figures say age 0-14, but this is only 3 ages, it will not work
  # then in text they say calculate age ratio for ages 0-24 AND then smooth only ages 0-19
  # I use the second option but there is a clear contradiction in text and 
  # I`m not sure it is exactly right
  # TR: OK, we leave this note here and can ask about it in future.
  
  kids <- dat_5 %>%
    filter(Age < 20)
  
  adults <- dat_5 %>% # ages 15 to Inf
    filter(Age > 10)
  # calculate the age ratio score before smoothing separately for kids and adults
  
  # Only for age score, we add one additional age to kids
  rsc_kids <- dat_5 %>%
    filter(Age < 24)
  
  age_rat_score_kids <- ageRatioScore(Value = rsc_kids$Pop,
                                      Age   = rsc_kids$Age) # check this
  
  # for adults use ages 15-19 : 70-74 for score calculation
  rsc_adults <- adults %>%
    filter(Age < 75)
  
  age_rat_score_adults <- ageRatioScore(
    Value  = rsc_adults$Pop,
    Age    = rsc_adults$Age,
    ageMin = min(rsc_adults$Age)
  )
  
  # smooth data with mav = 2 for adults
  dat5_mav_adults <- mav(
    Value = adults$Pop,
    Age   = adults$Age,
    n     = 2,
    tails = TRUE
  )
  
  # calculate the age ratio score after smoothing for adults
  age_rat_adults_2 <- tibble(Pop = dat5_mav_adults,
                             Age = names(dat5_mav_adults)) %>%
    mutate(Age = as.numeric(Age)) %>%
    filter(Age < 75)
  
  age_rat_score_adults_2 <-
    ageRatioScore(
      Value  = age_rat_adults_2$Pop,
      Age      = age_rat_adults_2$Age,
      ageMin   = min(age_rat_adults_2$Age)
    )
  
  # calculate the smoothing n for adults
  adult_n <- tibble(unsm = age_rat_score_adults,
                    sm   = age_rat_score_adults_2) %>%
    mutate(n = case_when(unsm < 4  ~ 1,
                         sm   < 4  ~ 2,
                         sm   >= 4 ~ 4)) %>%
    pull(n)
  
  # calculate the smoothing n for kids
  kids_n <- tibble(unsm = age_rat_score_kids) %>%
    mutate(n = ifelse(age_rat_score_kids < 4, 1, 2)) %>%
    pull(n)
  
  # smooth kids
  data_kids   <- mav(
    Value = kids$Pop,
    Age   = kids$Age,
    n     = kids_n,
    tails = TRUE
  )
  
  # smooth adults
  data_adults <- mav(
    Value = adults$Pop,
    Age   = adults$Age,
    n     = adult_n,
    tails = TRUE
  )
  
  # combine NOTE redistribute with linear weight assumption
  # NOTE: for 5 year ages there is only 1 age group that blends 15-19
  # Values are exactly the same, I do not understand why I need to blend them
  # So I just pick up one
  data_full <- c(data_kids[-length(data_kids)], data_adults)
  
  dt_fn <- structure(data_full,
                     .Names = names(data_full))
  
  # TR: this graduation shouldn't happen in here? Just spit back 5-year ages?
  # graduate to single ages
  final <- graduate_mono(dt_fn, OAG = TRUE)
  
  return(final)
  
}

# works
# Note that kids are being smoothed less than adults. As is recommended in the text.
# We see the artificial bulge due to under counting of children pop
# we might want to adjust it with the basepop_five function in future
# NOTE graph is per million
# tibble(old = pop1m_ind,
#        Age = 0:100,
#        new = gradute_data(dat)) %>%
#   ggplot(aes(x = Age, y = old / 1000000)) +
#   geom_line(linewidth = 0.6) +
#   geom_point(size = 2) +
#   geom_line(aes(x = Age, y = new / 1000000),
#             col = "red",
#             linewidth = 0.8) +
#   theme_light() +
#   theme(
#     legend.position = "none",
#     axis.title = element_blank(),
#     axis.text = element_text(color = "black", size = 12)
#   )