library(DemoTools)
library(tidyverse)
library(LifeIneq)
library(testthat)
library(scales)
library(covr)
source("R/readers.R")
source("R/list_utils.R")
source("R/lifetables.R")

# single
data_in <- read_csv("inst/extdata/single_hmd_spain.csv") %>%
  dplyr::select(-1)
# abriged
data_in <- read_csv("inst/extdata/abridged_hmd_spain.csv") %>% 
  dplyr::select(-1)
# 5-year
data_in <- read_csv("inst/extdata/five_hmd_spain.csv") %>%
  dplyr::select(-1)

argums <- expand_grid(
  Sex = "t",
  extrapFrom = c(60, 80),
  extrapLaw = c("Kannisto", "Kannisto_Makeham", 
                "Makeham", "Gompertz", 
                "GGompertz", "Beard", 
                "Beard_Makeham", "Quadratic"),
  OAnew = c(100, 80, 60),
  age_out    = c("single", "abridged", "5-year"), 
  radix = c(1, 100000),
  SRB = 1.05,
  a0rule = c("Andreev-Kingkade",
             "Coale-Demeny"),
  axmethod = c("UN (Greville)", "PASEX"),
  by_args = "Year"  
)


# lt_flexible(
#   data_in,
#   OAnew      = argums$OAnew[1],
#   age_out    = argums$age_out[1],
#   extrapFrom = argums$extrapFrom[1],
#   extrapFit  = NULL,  # Default NULL, computed later
#   extrapLaw  = argums$extrapLaw[1],
#   radix      = argums$radix[1],
#   SRB        = argums$SRB[1],
#   a0rule     = argums$a0rule[1],
#   axmethod   = argums$axmethod[1],
#   Sex        = argums$Sex[1],
#   by_args    = argums$by_args[1]
# )


# save results
x      <- vector(mode   = "list", 
                 length = nrow(argums))
# for trycatch
errors <- vector("numeric")

# Loop through each row in argums
for (i in 1:nrow(argums)) { 
  x[[i]] <- tryCatch(
    {
      lt_flexible(
        data_in,
        OAnew      = argums$OAnew[i],
        age_out    = argums$age_out[i],
        extrapFrom = argums$extrapFrom[i],
        extrapFit  = NULL,  # Default NULL, computed later
        extrapLaw  = argums$extrapLaw[i],
        radix      = argums$radix[i],
        SRB        = argums$SRB[i],
        a0rule     = argums$a0rule[i],
        axmethod   = argums$axmethod[i],
        Sex        = argums$Sex[i],
        by_args    = argums$by_args[i]
      )
    },
    error = function(e) {
      # Store the index and error message in the `errors` list
      errors[[i]] <- paste("Error at index", i, ":", e$message)
      NA  # Return NA for this entry in `x`
    }
  )
}

# name the resulting list
nms <- argums %>% 
  unite("x", everything(), sep = ", ") %>% 
  pull(x)
# add element number to names
nms <- str_c(1:nrow(argums), nms, sep = ", ")

# set the names
x <- x %>% 
  set_names(nms)

# lets see which names are resulting in problems
# some of the rough_method
# if no erros is empty and returns error
z <- x %>%
  keep(~ is.logical(.)) %>% 
  names() %>% 
  as.data.frame() %>%
  separate_wider_delim('.', 
                       delim = ", ", 
                       names_sep = "_", 
                       too_few = "align_start") %>% 
  set_names(c("element", names(argums)))


x[[1]]$data_out %>% 
  filter(map_lgl(data, is.data.frame)) %>%
  unnest(data) %>% 
  view()

# in lifetables do the sex imputation
# also allow for Mx usage in arguments
