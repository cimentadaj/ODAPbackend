# library(DemoTools)
# library(tidyverse)
# library(scales)
# library(readxl)
library(devtools)
load_all()
# source("R/readers.R")
# source("R/checkers.R")
# source("R/lifetables.R")
# source("R/plots.R") # broken function in this one
# 


Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
               247473,223014,172260,149338,127242,105715,79614,53660,
               31021,16805,8000,4000,2000,1000)

Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
            1335,3257,2200,4023,2167,4578,2956,4212,
            2887,2351,1500,900,500,300)

abridged_data <- tibble(Deaths = Deaths, 
                        Exposures = Exposures, 
                        Age = c(0, 1, seq(5, 100, by = 5)),
                        AgeInt = c(diff(Age), NA),
                        Sex = "Male")

write_delim(abridged_data,"data/abridged_data2.csv",";")
write_csv(abridged_data,"inst/extdata/abridged_data.csv") ### here
write_tsv(abridged_data,"data/abridged_data.tsv")
abridged_data
devtools::check()
# this is fed to us from the user
user_file <- "abridged_data.csv" ### 1
user_file <- "abridged_data.tsv" ### 1
user_file <- "abridged_data2.csv" # semicolon separated
user_file <- "abridged_data1.xlsx"
user_file <- "abridged_data2.xls"
user_file <- "abridged_data2.txt" # to check that function in NOT working with this format

data <- read_data("abridged_data.csv") # good
data <- read_data("abridged_data.tsv") # should we allow it?
data <- read_data("abridged_data2.csv")
data <- read_data("abridged_data1.xlsx")
data <- read_data("abridged_data2.xls")
# we expect an error.... but what if it's delimited?? Shall we just allow it?
data <- read_data("abridged_data2.csv")



# task 3: # We need to make predefined lists of valid values for each argument
# think also of what the user should SEE in the menu versus what value we need to 
# pass to the LT function. they don't need to be the same. First we do abridged.

check_data(data)

plot_the_initial_data(data)

check_heaping_general(data)

# Task 4: make a ggplot code snippet showing the incoming rates as a line
# and the outgoing rates with a dashed line in a different color, potentially
# only starting at the extrapFrom age. This function anticipates the output
# of lt_abridged().

# TR: wrap this in calc_lt() 
# Idea: we can detect whether incoming ages are abridged or single. The user should just need to select whether they want abridged or single outgoing ages. Make sense?

# So out wrapper function would be calc_lt(), passing in all arguments.
# we have lt_single2abridged() and lt_abridged2single(), for instance for
load_all()
data1 <- lt_flexible(Deaths = data$Deaths,
                     Exposures = data$Exposures,
                     Age = data$Age,
                     OAnew = 110,  
                     extrapFrom = 80,
                     extrapFit = seq(70,100,by=5),
                     age_out = "single")

OAnew      = 100   # TR element of basic, gets passed in
extrapFrom = 70    # TR element of advanced, gets passed in
extrapFit  = seq(70, 100, by = 5) # TR element of advanced, gets passed in
Mx_emp <- abridged_data$Deaths/ abridged_data$Exposures

#### lt check
data_out <- 
  lt_flexible(Deaths    = data$Deaths, 
              Exposures = data$Exposures,
              Age       = data$Age,
              OAnew     = 100,
              age_out = "single",  
              extrapFrom = 70,
              extrapFit = data$Age[data$Age >= 60], 
              radix     = 1e+05,
              extrapLaw = NULL,
              SRB       = 1.05,
              a0rule    = "ak",
              axmethod  = "un",
              Sex       = "m")
make_figure(data, data_out, 70)
plot_initial_single_sex(data)
# plot check
# a little data "simulation"
data1           <- data
data1$Sex       <- "Female"
data1$Exposures <- data1$Exposures
data1$Deaths    <- data1$Deaths
data <- data %>% 
  full_join(data1) %>%
  mutate(Deaths = ifelse(Sex == "Female", Deaths + rpois(22, lambda = 50), Deaths))

# works
make_figure(data, data_out)
# basic
basic <- 
  tibble(for_us = c("OAnew",
                    "OAG",
                    "sex",
                    "radix"),
         for_users = c("Desired open age group",
                       "Whether or not the last element of nMx is an open age group.",
                       "Sex",
                       "Radix value"),
         default_calue = c("100",
                           "TRUE",
                           "male",
                           "100000"),
         look = c("field with numeric input",
                  "a box to tick",
                  "a box to tick",
                  "field with numeric input"))

# advanced
advanced <- 
  tibble(for_us = c("extrapLaw",
                    "extrapFrom",
                    "extrapFit",
                    "SRB",
                    "a0rule",
                    "axmethod"),
         for_users = c("Parametric mortality law for LT extrapolation",
                       "Age from which to impute extrapolated mortality",
                       "Ages to include in model fitting.",
                       "The sex ratio at birth (boys / girls)",
                       "The rule for modelling a0 value",
                       "The method to model ax values"),
         default_calue = c("Kannisto if < 90, or makeham",
                           "80",
                           "60",
                           "1.05",
                           "ak",
                           "un"),
         look = c("The dropping list with choices",
                  "a field with numeric input",
                  "filed with numeric input",
                  "field with numeric input",
                  "two coice buttons",
                  "two coice buttons"))


write.csv(basic, file = "basic.csv")




devtools::load_all()
 Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
 247473,223014,172260,149338,127242,105715,79614,53660,
 31021,16805,8000,4000,2000,1000)
 
 Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
             1335,3257,2200,4023,2167,4578,2956,4212,
             2887,2351,1500,900,500,300)

Age = c(0, 1, seq(5, 100, by = 5))
 data_out <- 
   lt_flexible(Deaths    = Deaths, 
               Exposures = Exposures,
               Age       = Age,
               OAnew     = 100,
               age_out   = "single",  
               extrapFrom = 80,
               extrapFit = Age[Age >= 60], 
               radix     = 1e+05,
               extrapLaw = NULL,
               SRB       = 1.05,
               a0rule    = "ak",
               axmethod  = "un",
               Sex       = "m")
 plot_lifetable(data_out = data_out)



write.csv(advanced, file = "advanced.csv")


Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
                              247473,223014,172260,149338,127242,105715,79614,53660,
                              31021,16805,8000,4000,2000,1000)
               
Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
                           1335,3257,2200,4023,2167,4578,2956,4212,
                           2887,2351,1500,900,500,300)
               
data <- tibble(Deaths = Deaths, 
                                       Exposures = Exposures, 
                                       Age = c(0, 1, seq(5, 100, by = 5)))




load_all()
library(tidyverse)
library(devtools)
library(DemoTools)
data(pop1m_ind, package = "DemoTools")
Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
               247473,223014,172260,149338,127242,105715,79614,53660,
               31021,16805,8000,4000,2000,1000)

Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
            1335,3257,2200,4023,2167,4578,2956,4212,
            2887,2351,1500,900,500,300)

data_abr_in <- tibble(Deaths = Deaths, 
                        Exposures = Exposures, 
                        Age = c(0, 1, seq(5, 100, by = 5)),
                        AgeInt = c(diff(Age), NA),
                        Sex = "Male")

data1_in <- data.frame(Exposures = pop1m_ind,
                     Age = 0:100)
data5_in <- data.frame(Exposures = groupAges(pop1m_ind, N = 5),
                      Age = seq(0, 100, 5))
data_other1 <- data5_in |> 
  mutate(Age = ifelse(Age > 50, Age - Age %% 10, Age)) |> 
  summarize(Exposures = sum(Exposures), .by = Age)

data_other1 <- data5_in |> 
  mutate(Age = ifelse(Age > 50, Age - Age %% 10, Age)) |> 
  summarize(Exposures = sum(Exposures), .by = Age)

# big tests

fine_methods <-   c("auto", "none", "sprague",
                                  "beers(ord)", "beers(mod)",
                                  "grabill", "pclm", "mono",
                                  "uniform")
rough_methods = c("auto", "none", "Carrier-Farrag",
                 "KKN", "Arriaga", "United Nations",
                 "Strong", "Zigzag")
age_ins <- c("single","5-year","abridged","other")
age_outs <- c("single","5-year","abridged")


beefy_output <- list()
for (ai in age_ins ){
   
  data_in <- switch(ai,
                    "single" = data1_in,
                    "5-year" = data5_in,
                    "abridged" = data_abr_in,
                    "other" = data_other1)
  for (ao in age_outs){
    for (fm in fine_methods){
      for (rm in rough_methods){
        data_out <- try(smooth_flexible(data_in,
                                    variable = "Exposures", 
                                    rough_method = rm,
                                    fine_method = fm, 
                                    constrain_infants = TRUE, 
                                    age_out = ao, 
                                    u5m = .1,
                                    Sex = "t"))
        labeli <- paste(ai, ao, fm, rm, sep = "-")
        beefy_output[[labeli]] <- data_out
        
      }
    }
  }
}
length(beefy_output)
ind <- lapply(beefy_output, class) |> unlist() %>% '=='("try-error")
sum(ind)
dfind <- lapply(beefy_output, is.data.frame) |> unlist()
 beefy_output[!dfind] |> names()

data(pop1m_ind, package = "DemoTools")
data_in <- data.frame(Exposures = pop1m_ind,
                      Age       = 0:100)

