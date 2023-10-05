library(DemoTools)
library(tidyverse)
library(xlsx)
library(scales)

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
user_file <- "abridged_data.csv" ### 1
user_file <- "abridged_data1.xlsx"
user_file <- "abridged_data2.xls"
user_file <- "abridged_data2.txt" # to check that function in NOT working with this format

# file_type <- tools::file_ext(file.path("data", user_file))
# str_split(user_file, pattern = "\\.") |> unlist() |> rev() %>% '['(1)

# allowable_extensions <- c("csv","xlsx","xls")
# ind <- str_detect(user_file, pattern = paste0(".", allowable_extensions,"$"))

# then we need to read it in
# RT task 1: make reader function
# Maybe also check if some rows should be skipped??
read_data <- function(user_file) { 
  
  # check the extension. Can be made external
  extension_check <- function(user_file) { 
    
    allowable_extensions <- c("csv","xlsx","xls")
    ind                  <- str_detect(user_file, 
                                       pattern = paste0(".", allowable_extensions,"$"))
    allowable_extensions[ind]
  }
  
  # now we know the extension and we can proceed with reading the file 
  extension <- extension_check(user_file)
  
  # if extension is not allowed, throw an error
  stopifnot("Error. The allowable extenction was not detected. Please provide the data in .csv, .xlsx, or .xls format" 
            = length(extension) == 1)
  
  # read csv ort excel data
  if(extension == "csv") {
    
    data <- read_delim(str_c("data/", user_file), show_col_types = FALSE)
    
    
  } else { 
    # can handle both xls and xlsx data. 
    # we can use readxl if we want to hard code format
    # assumes the data is on a first sheet
    
    data <- read.xlsx(str_c("data/", user_file), sheetIndex = 1)
    
  } 
  
  # calculate empirical nmx
  data <- data %>% 
    mutate(Mx_emp = Deaths / Exposures)
  
  return(data)
}

# Done. Now we have a downloaded data and we proceed to chect if all we want is present
# check if the data stars with the empty rows 
data <- read_data(user_file)

# TR task 2: make checker function
# is_abridged()
# check for NAs
# check for scale (leave note and this code commented out)
# hackish check for plausible ranges
# abridged_data |> 
#   mutate(mx = Deaths / Exposures,
#          AgeInt = if_else(is.na(AgeInt), 10, AgeInt)) |> 
#   reframe(e0 = sum(exp(-cumsum(rep(mx, times = AgeInt)))) + .5)

# Lets break everything in a little check functions
# alternative. asserive package
check_numeric <- function(data) { 
  
  isnumeric <- data %>%
    map(~ is.numeric(.)) %>% 
    unlist()
  
  if(sum(isnumeric) < 4) { 
    
    stop("Please check the input data. Every column should be numeric, while columns", " are not.")
    
  }
  
}

check_missing_cols <- function(data) { 
  
  missing_cols <- setdiff(c("Deaths", "Exposures","Age", "AgeInt"), names(data))
  
  if(ncol(data) < 4) { 
    
    
    stop(str_c("The following columns are missing from the data:- ", missing_cols, ". The calculations are halted."))
    
  }
  
}

check_rows <- function(data) { 
  
  if(nrow(data) < 10) { 
    
    stop("The number of rows in the datast is less than expected. Please check the data before proceeding with calculations")
    
  }
  
}

check_abridged <- function(data) { 
  
  if(!(is_abridged(data$Age))) {
    
    stop("You vector of ages is not of the lower bounds of abridged age groups. Please fix the data before proceeeding")
    
  }
  
}

check_nas <- function(data) { 
  
  nas <- is.na(data) |>
    colSums()
  
  if(sum(nas) > 0) { 
    stop("Following columns have missing data:- ", 
         names(data)[nas > 0],
         ", n = ",
         nas[nas > 0],
         ". Please ensure there are no mistakes.")
  }
  
}

# not sure it is needed. Maybe better display line by line. This combiines them all
check_data <- function(data) { 
  
  check_numeric(data)
  check_missing_cols(data)
  check_rows(data)
  check_abridged(data)
  check_nas(data)
  
}

check_data(data)


# task 3: # We need to make predefined lists of valid values for each argument
# think also of what the user should SEE in the menu versus what value we need to 
# pass to the LT function. they don't need to be the same. First we do abridged.

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



# Task 4: make a ggplot code snippet showing the incoming rates as a line
# and the outgoing rates with a dashed line in a different color, potentially
# only starting at the extrapFrom age. This function anticipates the output
# of lt_abridged().
data1 <- lt_abridged(Deaths = abridged_data$Deaths,
                     Exposures = abridged_data$Exposures,
                     Age = abridged_data$Age,
                     OAnew = 100,
                     extrapFrom = 70,
                     extrapFit = seq(70,100,by=5))

OAnew      = 100
extrapFrom = 70
extrapFit  = seq(70, 100, by = 5)

# I made it a litttle fancy. SInce the data from the function are expected to be provided I had coded the values
# If needed can be turned into a function with compled return()
ggplot() + 
  geom_line(data = data, aes(x = Age, y = Mx_emp), linewidth = 0.8) + 
  geom_line(data = filter(data1, Age >= extrapFrom), aes(x = Age, y = nMx), lty = 2, col = "red", linewidth = 1) +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_log10(labels = label_log(digits = 2)) +
  theme_light() +
  geom_vline(xintercept = extrapFrom, lty = 2)+
  labs(x = "Age",
       y = "nMx",
       subtitle = "The difference between the empirical Mx and the extrapolated values for a given age range on a log10 scale.")+
  theme(axis.text = element_text(color = "black"),
        plot.subtitle = element_text(size = 12, color = "black"))