library(DemoTools)
library(tidyverse)
library(LifeIneq)
library(testthat)
library(scales)
library(covr)
source("R/readers.R")
source("R/list_utils.R")
source("R/graduation.R")

# single
data_in <- read_csv("inst/extdata/single_hmd_spain.csv") %>%
  dplyr::select(-1)
# abriged
data_in <- read_csv("inst/extdata/abridged_hmd_spain.csv") %>% 
  dplyr::select(-1)
# 5-year
data_in <- read_csv("inst/extdata/five_hmd_spain.csv") %>%
  dplyr::select(-1)

# argumets used in graduation function
argums <- expand_grid(
  variable = c("Deaths", "Exposures"),
  age_out  = c("single", "abridged", "5-year"),
  fine_method = c(
    "auto",
    "none",
    "sprague",
    "beers(ord)",
    "beers(mod)",
    "grabill",
    "pclm",
    "mono",
    "uniform"
  ),
  rough_method = c(
    "auto",
    "none",
    "Carrier-Farrag",
    "KKN",
    "Arriaga",
    "United Nations",
    "Strong",
    "Zigzag"
  ),
  u5m               = 1.05,
  constrain_infants = TRUE,
  Sex = "t",
  by_args = "Year"
)

# save results
x      <- vector(mode   = "list", 
                 length = nrow(argums))
# for trycatch
errors <- vector("numeric")

# Loop through each row in argums
for (i in 1:nrow(argums)) { 
  x[[i]] <- tryCatch(
    {
      smooth_flexible(
        data_in,
        variable          = argums$variable[i],
        age_out           = argums$age_out[i],
        fine_method       = argums$fine_method[i],
        rough_method      = argums$rough_method[i],
        u5m               = argums$u5m[i],
        constrain_infants = argums$constrain_infants[i],
        Sex               = argums$Sex[i],
        by_args           = argums$by_args[i]
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
z <- x %>%
  keep(~ is.logical(.)) %>% 
  names() %>% 
  as.data.frame() %>%
  separate_wider_delim('.', 
                       delim = ", ", 
                       names_sep = "_", 
                       too_few = "align_start") %>% 
  set_names(c("element", names(argums)))

# lest confirm
tst <- data_in %>% 
  filter(Sex == "Male")

# turn to proper 5 year data
tst <- tst %>%
  mutate(Age = .data$Age - .data$Age %% 5) |> 
  group_by(Age)  |> 
  summarize(Deaths = sum(Deaths))

# negatives again
smooth_age_5(Value  = tst$Deaths,
             Age    = tst$Age,
             method = "Arriaga")

smooth_age_5(Value  = tst$Deaths,
             Age    = tst$Age,
             method = "Strong")

# check auto auto works good
names(x)[1]
x[[1]]$figures$`1_2012`$figure
x[[1]]$figures$`2_2012`$figure
x[[1]]$figures$`2_2012`$figure

# generic methods not too much Carrier-Farrag single
names(x)[3]
x[[3]]$figures$`1_2012`$figure
x[[3]]$figures$`2_2012`$figure
x[[3]]$figures$`2_2012`$figure
# so now we know that generic rough_methods 
# may cause negative values with smooth_age_5
# KKN, Arriaga, Strong, UN, so far