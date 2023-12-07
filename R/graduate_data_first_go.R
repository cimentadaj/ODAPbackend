# packages
library(tidyverse)
library(DemoTools)

# test data
dat <- tibble(Pop = pop1m_ind,
              Age = 0:100)

#' gradute_data
#' @description Smoothes the population counts using the moving averages. The methods arae adpoted from the "Method protocol for the evaluation of census population data by age and sex"
#' @param dat tibble. A tibble with two columns - `Pop` - population counts and `Age` provided in single age intervals, 5 year age intervals or abridged format e.g. with ages 0, 1-4, 5-9 etc
#' @return A named vector with graduated and smoothed population counts
#' @importFrom dplyr case_when mutate group_by mutate_all filter pull between
#' @importFrom tibble tibble
#' @importFrom DemoTools is_single check_heaping_bachi groupAges ageRatioScore mav graduate_mono
#' @export
#' @examples
#' \dontrun{
#' dat <- tibble(Pop = pop1m_ind,
#' Age = 0:100)
#'
#' gradute_data(dat = dat)
#' }
#' 
gradute_data <- function(dat) { 

# 1) check if data comes in single
sngl <- is_single(dat$Age)

# 2) calulate adult bachi 
bachi <- check_heaping_bachi(dat$Pop, 
                             Age     = dat$Age, 
                             ageMin  = 23, 
                             ageMax  = 77, 
                             method  = "pasex", 
                             details = TRUE)

# This is my attempt to catch the abridged data e.g ages 0 and 1-4 and separate it from 0-4
# if age is single we definitely omit this in all other cases - 
# I assume that the ratio of population at first and second age will exceed 2.6
# only if the first age is 0 and second age is 1-4. otherwise for 0-4 and 5-9 it should be still less than 2.6
check_abridged <- (dat$Pop[1] / dat$Pop[2]) > 2.6

if(!sngl & check_abridged) { 
  
  dat <- dat %>% 
    mutate(Age = c(0, 0, (.$Age)[-c(1:2)])) %>% 
    group_by(Age) %>% 
    summarise(POp = sum(Pop), .groups = "drop")
  
  }

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
mxprop2 <- (sum(sort(pct, decreasing = TRUE)[c(1, 2)]) - 20 ) / index

# 3 different conditions
# 1) if ages are single and bachi is more than or equal to 30
# is this case group data into 5 year ages and graduate accordingly
# # case 3 is essentially case 2 with no need for preliminary grouping.
# For this I add 2 if conditions, that will separate the 2 cases. 
# The only difference in implementation is that in case 1 we group ages and in case 2 we do not
if(sngl & index >= 30 | !sngl) {

  # check condition for case 1 if TRUE group ages
  if(sngl & index >= 30) { 
    
  # group data in 5 years
    
    cmbn_5_yrs <- groupAges(dat$Pop, N = 5)
    dat_5 <- tibble(Pop = cmbn_5_yrs,
                    Age = names(cmbn_5_yrs)) %>% 
      mutate_all(as.numeric)
  
  }
  
  # if the data is already grouped skip step one
  if(!sngl) { 
    
    dat_5 <- dat %>% 
      mutate_all(as.numeric)
    
  }
  
  # separate data into kids and adults
  # figures say age 0-14, but this is only 3 ages, it will not work
  # then in text they say calculate age ratio for ages 0-24 AND then smooth only ages 0-19
  # I use the second option but there is a clear contradiction in text and I`m not sureit is exactly right
  kids <- dat_5 %>% 
    filter(Age < 20)
  
  adults <- dat_5 %>% # ages 15 to Inf
    filter(Age > 10)
  # calculate the age ratio score before smoothing separately for kids and adults
  
  # Only for age score
  rsc_kids <- dat_5 %>% 
    filter(Age < 24)
  
  age_rat_score_kids <- ageRatioScore(Value = rsc_kids$Pop, 
                                      Age   = rsc_kids$Age) # check this
  
  # for adults use ages 15-19 : 70-74 for score calculation
  rsc_adults <- adults %>% 
    filter(Age < 75)
  
  age_rat_score_adults <- ageRatioScore(Value  = rsc_adults$Pop, 
                                        Age    = rsc_adults$Age, 
                                        ageMin = min(rsc_adults$Age))
  
  # smooth data with mav = 2 for adults
  dat5_mav_adults <- mav(Value = adults$Pop, 
                         Age   = adults$Age, 
                         n     = 2, 
                         tails = TRUE)
  
  # calculate the age ratio score after smoothing for adults
  age_rat_adults_2 <- tibble(Pop = dat5_mav_adults,
                             Age = names(dat5_mav_adults)) %>%
    mutate_all(as.numeric) %>% 
    filter(Age < 75)
  
  age_rat_score_adults_2 <- ageRatioScore(Value  = age_rat_adults_2$Pop, 
                                        Age      = age_rat_adults_2$Age, 
                                        ageMin   = min(age_rat_adults_2$Age))  
  
  # calculate the smoothing n for adults
  adult_n <- tibble(unsm = age_rat_score_adults,
                    sm   = age_rat_score_adults_2) %>%
    mutate(n = case_when(
      unsm < 4  ~ 1,
      sm   < 4  ~ 2,
      sm   >= 4 ~ 4
    )) %>% 
    pull(n)
  
  # calculate the smoothing n for kids
  kids_n <- tibble(unsm = age_rat_score_kids) %>% 
    mutate(n = ifelse(age_rat_score_kids < 4, 1, 2)) %>% 
    pull(n)
  
  # smooth kids
  data_kids   <- mav(Value = kids$Pop,   
                     Age   = kids$Age,   
                     n     = kids_n, 
                     tails = TRUE)
  
  # smooth adults
  data_adults <- mav(Value = adults$Pop, 
                     Age   = adults$Age, 
                     n     = adult_n, 
                     tails = TRUE)
  
  # combine NOTE redistribute with linear weight assumption
  # NOTE: for 5 year ages there is only 1 age group that blends 15-19
  # Values are exactly the same, I do not understand why I need to blend them
  # So I just pick up one
  data_full <- c(data_kids[-length(data_kids)], data_adults)
  
  
  dt_fn <- structure(data_full, 
                     .Names = names(data_full))
  
  # graduate to single ages
  final <- graduate_mono(dt_fn, OAG = TRUE)
  
} 

# if ages single but bachi is less than to 30
if(sngl & index < 30) { 
  
  # We dont have years of education. So I always choose the maximum mav of the two available
  # calulate n
  n <- tibble(prp0and5,
              index,
              mxprop2) %>% 
    mutate(n = case_when(
      between(index,    4, 7.99999) & prp0and5 > 0.65 ~ 10,
      between(index,    4, 7.99999) & prp0and5 < 0.65 ~ 6,
      between(index,    2, 3.99999) & prp0and5 > 0.60 ~ 6,
      between(index,    2, 3.99999) & prp0and5 < 0.60 ~ 4,
      between(index, 0.75, 1.99999) & mxprop2  > 0.70 ~ 4,
      between(index, 0.75, 1.99999) & mxprop2  < 0.70 ~ 2,
      between(index, 0,  0.7499999) & mxprop2  < 0.55 ~ 2,
      between(index, 0,  0.7499999) & mxprop2  > 0.55 ~ 1,
      between(index, 8,  29.99999)                    ~ 10 
    )) %>% 
    pull(n)
  
  # smooth accordingly
  # NOTE: on pictures I do not see the division into adults and kids, but maybe it is needed here too
  final <- mav(Value = dat$Pop, 
               Age   = dat$Age, 
               n     = n, 
               tails = TRUE)
  
}

return(final)

}

# works
# Note that kids are being smoothed less than adults. As is recommended in the text.
# We see the artificial bulge due to under counting of children pop
# we might want to adjust it with the basepop_five function in future
# NOTE graph is per million
tibble(old = pop1m_ind,
       Age = 0:100,
       new = gradute_data(dat)) %>% 
  ggplot(aes(x = Age, y = old / 1000000)) + 
  geom_line(size = 0.6) + 
  geom_point(size = 2) + 
  geom_line(aes(x = Age, y = new / 1000000),col = "red", linewidth = 0.8) +
  theme_light() + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_text(color = "black", size = 12))

