library(DemoTools)
library(tidyverse)
library(scales)
library(readxl)
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
write_delim(abridged_data,"data/abridged_data2.csv",";")
write_csv(abridged_data,"data/abridged_data.csv")
write_tsv(abridged_data,"data/abridged_data.tsv")
abridged_data

# this is fed to us from the user
user_file <- "abridged_data.csv" ### 1
user_file <- "abridged_data.tsv" ### 1
user_file <- "abridged_data2.csv" # semicolon separated
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
  stopifnot("File extension not supported at this time. Please provide the data in .csv, .xlsx, or .xls format" =length(extension) == 1 )
  
  # read csv ort excel data
  if(extension == "csv") {
    # TR: changed to file.path() because the path separator is then 
    # OS dependent
    
    # For read_delim() no need to specify delim, it's apparently detected; I tried
    # , ; \t
    data <- read_delim(file.path("data", user_file), show_col_types = FALSE)
    
    
  } else { 
    # can handle both xls and xlsx data. 
    # we can use readxl if we want to hard code format
    # assumes the data is on a first sheet
    # TR: I think read_excel does both formats too and can handle flexible 
    # positioning
    
    data <- read_excel(file.path("data", user_file), sheet = 1)
    
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
# alternative. assertive package

# TR: note NA is the value we use for AgeInt in the open age group
check_numeric <- function(data) { 
  
  isnumeric <- data %>%
    map(~ is.numeric(.)) %>% 
    unlist()
  
  if(sum(isnumeric) < 4) { 
    
    stop("Please check the input data. Every column should be numeric, while columns", " are not.")
    
  }
  
}

check_missing_cols <- function(data) { 
  # TR: DemoTools has the function age2int() to infer age intervals from an Age vector,
  # to technically we don't need it. We do however need ages specified as lower bounds of
  # abridged age groups (for the abridged lifetable function anyway). Let's not insist on
  # AgeInt being given.
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

# TR: see also DemoTools functions:
# is_age_coherent()
check_coherent <- function(data) { 
  
  tst <- is_age_coherent(data$Age, data$AgeInt)
  
  stopifnot("The age is not coherent" = tst)
  
}

# is_age_sequential()
check_sequential <- function(data) { 
  
  tst <- is_age_sequential(data$Age)
  
  stopifnot("The age is not sequential" = tst)
  
}

# is_age_redundant()
check_redundant <- function(data) { 
  
  tst <- is_age_redundant(data$Age, data$AgeInt)
  
  stopifnot("Provided age data is redundadt" = !tst)
  
}

parse_number("40-44")
# TR add a check that the lowest age is 0. 

check_lower <- function(data) { 

stopifnot("Age should start with 0." = min(data$Age) == 0)

}
  
# Currently the lt_abridged function assumes this,
# although it would be nice to generalize it to allow for truncated age ranges.
# TR: check_abridged() only relevant for lt_abridged(), so that can happen elsewhere.
# not sure it is needed. Maybe better display line by line. This combines them all
check_data <- function(data) { 
  
  check_numeric(data)
  check_missing_cols(data)
  check_rows(data)
  # check_abridged(data) # replace it or remove
  check_nas(data)
  check_lower(data)
  check_coherent(data)
  check_sequential(data)
  check_redundant(data)
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

# TR: wrap this in calc_lt() 
# Idea: we can detect whether incoming ages are abridged or single. The user should just need to select whether they want abridged or single outgoing ages. Make sense?

# So out wrapper function would be calc_lt(), passing in all arguments.
# we have lt_single2abridged() and lt_abridged2single(), for instance for

data1 <- lt_abridged(Deaths = abridged_data$Deaths,
                     Exposures = abridged_data$Exposures,
                     Age = abridged_data$Age,
                     OAnew = 100,  
                     extrapFrom = 70,
                     extrapFit = seq(70,100,by=5))

OAnew      = 100   # TR element of basic, gets passed in
extrapFrom = 70    # TR element of advanced, gets passed in
extrapFit  = seq(70, 100, by = 5) # TR element of advanced, gets passed in
Mx_emp <- abridged_data$Deaths/ abridged_data$Exposures
# I made it a little fancy. Since the data from the function are expected 
# to be provided I had coded the values
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


#### HEre it is

#' @param age_out character, either `"single"`, or `"abridged"`
lt_flexible <- function(Deaths    = Deaths, # replace with NULL. this is for demonstration purposes
                        Exposures = Exposures,
                        Age       = Age,
                        
                        # recall all of these are passed in from the app, which will contain
                        # its own default values.
                        OAnew     = 100,
                        age_out = "single", # CHECK! This is for option number 2 
                        etrapFrom = 80,
                        extrapFit = Age[Age >= 60], # maybe somehow modify the argument? Not sure if needed
                        radix     = 1e+05,
                        extrapLaw = NULL,
                        SRB       = 1.05,
                        a0rule    = "ak",
                        axmethod  = "un",
                        Sex       = "m") {
  # TR: no need to determine extrapLaw here, it happens
  # natively in the lt functions.


  
  age_in <- case_when(is_single(Age) ~ "single",
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
  if (age_in == "abridged" & age_out == "single"){
    data_out <- lt_abridged2single(Deaths     = Deaths,
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
  
  # age_in single, calculate no matter whether we 
  # keep single ages or want abridged output; if single
  # data_out doesn't change again
  if (age_in == "single"){
    # useful in case we use lt_single_mx()
    Mx_emp <- Deaths/ Exposures
    # Don't check age_out yet here, because the abridge function requires a 
    # precalculated lifetable, see below
    # TR same story; arg management should be complete and strategic
    data_out <- lt_single_mx(nMx        = Mx_emp,
                             Age        = Age,
                             OAnew      = OAnew,
                             extrapFrom = extrapFrom,
                             extrapFit  = extrapFit, # should we change it here too to 1 year intervals?
                             radix      = radix,
                             extrapLaw  = extrapLaw,
                             SRB        = SRB,
                             a0rule     = a0rule,
                             axmethod   = axmethod,
                             Sex        = Sex)
    
  }

  # final case, age_in single and age_out abridged,
  # requires the precalculated single-age lifetable
  if (age_in == "single" & age_out == "abridged"){

      data_out <- lt_single2abridged(lx  = data_out$lx, 
                                     nLx = data_out$nLx, 
                                     ex  = data_out$ex, 
                                     Age = data_out$Age)
      
  }
    
  # now all cases handled
  return(data_out)
  
}


# This can be more modular, as in a separate call, please adjust so that it can just take these two
# data objects? Or whatever you think makes sense. This won't currently run because I changed the args
# and scoping, but just so you see this can also be modular
make_figure <- function(data_out, data_in){
  # plot the results
  figure <- ggplot() + 
    geom_line(aes(x = Age, y = Mx_emp), linewidth = 0.8) + 
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
  
  
  # return a list with both data and figure
  return(lst(data   = out,
             figure = figure))
  
}

# check. Works
lt_flexible()


