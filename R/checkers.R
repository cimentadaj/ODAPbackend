# TR: add some roxygen-style documentation to these,
# and a manifest here at the top

extension_check <- function(user_file) { 
  # consider txt if it's delimited and reads correctly by read_delim()?
  # but we need to cathc failures intelligently in that case. A standard HMD
  # file would fail, for instance, but maybe it also should fail.
  allowable_extensions <- c("csv","xlsx","xls","tsv")
  ind                  <- str_detect(user_file, 
                                     pattern = paste0(".", allowable_extensions,"$"))
  allowable_extensions[ind]
}

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

# retired, now that we have a flexible lifetable function
# check_abridged <- function(data) { 
#   
#   if(!(is_abridged(data$Age))) {
#     
#     stop("You vector of ages is not of the lower bounds of abridged age groups. Please fix the data before proceeeding")
#     
#   }
#   
# }

# TR: needs to be modified to allow NA in final position of AgeInt, if there is 
# an AgeInt column...
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
  # make this return a data.frame with 3 columns
  # check | pass/fail | message
  # don't use stop or errors for these
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
