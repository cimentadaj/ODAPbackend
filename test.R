
library(DemoTools)
library(tidyverse)

?lt_abridged

Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
               247473,223014,172260,149338,127242,105715,79614,53660,
               31021,16805,8000,4000,2000,1000)

Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
            1335,3257,2200,4023,2167,4578,2956,4212,
            2887,2351,1500,900,500,300)

abridged_data <- tibble(Deaths = Deaths, 
                        Exposures = Exposures, 
                        Age = c(0, 1, seq(5, 100, by = 5)),
                        AgeInt = c(diff(Age), NA))
write_csv(abridged_data,"data/abridged_data.csv")
abridged_data

# this is fed to us from the user
user_file <- "abridged_data.csv"

# file_type <- tools::file_ext(file.path("data", user_file))
# str_split(user_file, pattern = "\\.") |> unlist() |> rev() %>% '['(1)

allowable_extensions <- c("csv","xlsx","xls")
ind <- str_detect(user_file, pattern = paste0(".", allowable_extensions,"$"))

# then we need to read it in

# RT task 1: make reader function
# TR task 2: make checker function
# is_abridged()
# check for NAs
# check for scale (leave note and this code commented out)
# hackish check for plausible ranges
# abridged_data |> 
#   mutate(mx = Deaths / Exposures,
#          AgeInt = if_else(is.na(AgeInt), 10, AgeInt)) |> 
#   reframe(e0 = sum(exp(-cumsum(rep(mx, times = AgeInt)))) + .5)

# task 3: # We need to make predefined lists of valid values for each argument
# think also of what the user should SEE in the menu versus what value we need to 
# pass to the LT function. they don't need to be the same. First we do abridged.

# Task 4: make a ggplot code snippet showing the incoming rates as a line
# and the outgoing rates with a dashed line in a different color, potentially
# only starting at the extrapFrom age. This function anticipates the output
# of lt_abridged().

lt_abridged(Deaths = abridged_data$Deaths,
            Exposures = abridged_data$Exposures,
            Age = abridged_data$Age,
            OAnew = 100,
            axtrapFrom = 90,
            extrapFit = seq(70,100,by=5))











