
# load packages to run this example
# TEMPORARY Thing
library(DemoTools)
library(tidyverse)
library(wpp2024)
library(ODAPbackend)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!! IMPORTANT NOTE:
# If you want to run the function
# the examples of implementation are provided in the end of the script
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ---------------------------------------------------------------------------- #
# We start with preparing the model data for testing
# ---------------------------------------------------------------------------- #
# prepare mx data as a dataframe
Pop <- pop1m_ind %>% 
  as.data.frame() %>%
  rownames_to_column() %>% 
  set_names(c("age", "Pop")) %>%
  mutate(sex = "M", 
         year = 1971, 
         name = "India") %>% 
  as_tibble() %>% 
  mutate(age = as.numeric(age) - 1)

# ---------------------------------------------------------------------------- #
# imagine we use India, 1971, males as in the example provided by DemoTools
data("mx1dt")
nLx <- mx1dt    %>%
  select(-mxB) %>% 
  as_tibble()  %>% 
  pivot_longer(c(mxM, mxF),
               names_to  = "sex",
               values_to = "mx") %>% 
  mutate(sex = str_sub(sex, start = 3)) %>% 
  filter(name == "India" | country_code == 356) %>% 
  filter(sex == "M", year == 1971) %>%
  group_by(name, country_code, sex, year) %>% 
  reframe(lt_single_mx(nMx = mx, Age = age)) %>% 
  select(country_code:sex, age = Age, nLx)


#' ODAP OPAG Mortality and Population Redistribution Analysis
#' 
#' This function prepares population data and mortality life table data (`nLx`) to perform
#' age redistribution using the OPAG method. It supports flexible input with country name/code,
#' sex, and year filters, and handles both user-provided and WPP standard mortality data.
#' It outputs adjusted population estimates along with diagnostic plots showing original vs redistributed populations.
#' 
#' @param Pop A data frame or tibble of population counts with columns including \code{age}, \code{pop}, and optionally \code{name}, \code{sex}, \code{year}, \code{country_code}.
#' @param Age_fit Numeric vector of two ages defining the age range for fitting redistribution (default \code{c(60, 70)}).
#' @param AgeInt_fit Numeric vector of two age interval widths corresponding to \code{Age_fit} (default \code{c(10, 10)}).
#' @param Redistribute_from Numeric scalar age threshold above which population redistribution occurs (default 80).
#' @param nLx Optional mortality life table data frame containing columns \code{age}, \code{nLx}, and grouping columns; if NULL, data is pulled from the latest installed WPP package.
#' @param name Character vector of country names to filter by (default \code{"India"}).
#' @param country_code Numeric vector of country codes to filter by (default \code{356}).
#' @param year Numeric vector of years to filter by (default \code{1971}).
#' @param sex Character scalar indicating sex to filter by, e.g. \code{"M"} or \code{"F"} (default \code{"M"}).
#' 
#' @details
#' The function automatically handles missing grouping columns in \code{Pop}, standardizes column names,
#' and validates age intervals. It retrieves mortality data if not provided, filters according to parameters,
#' and groups mortality rates if age intervals are broader than 1 year.
#' 
#' The OPAG function is applied per unique group to redistribute populations above \code{Redistribute_from} age.
#' Output includes both adjusted population estimates and ggplot2 objects visualizing redistribution.
#' 
#' @return A list with elements:
#' \describe{
#'   \item{\code{data_out}}{A named list of OPAG result objects by group.}
#'   \item{\code{figures}}{A named list of ggplot2 plots showing original and redistributed populations.}
#' }
#' 
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom purrr map map2
#' @export



# just a working name
odap_opag <- function(Pop               = NULL,
                      Age_fit           = c(60, 70),
                      AgeInt_fit        = c(10, 10),
                      Redistribute_from = 80,
                      OAnew             = 100,
                      method            = c("mono", "pclm", "uniform"),
                      nLx               = NULL,
                      # we want for user to be able to choose country 
                      # by name AND/OR code
                      name              = "India",
                      country_code      = 356,
                      # Here we indicate the sex and year to choose from
                      # latest wpp if needed
                      year              = 1971,
                      sex               = "M"
                      ) {
  
  # chosen method
  method <- match.arg(method, c("mono", "pclm", "uniform"))
  
  
  # Helper: conditional filtering for user defined variables
  # e.g. if sex  exits and provided by user we use it in filtering
  conditional_filter <- function(df, col, values) {
    
    if(!is.null(values) && col %in% names(df)) {
      
      df %>% 
        filter(.data[[col]] %in% values)
      
    } else df
    
  }
  
  # Helper: conditional arrange for user defined variables
  conditional_arrange <- function(df, cols) {
    # Keep only columns that exist in df
    cols_exist <- intersect(cols, names(df))
    
    if (length(cols_exist) > 0) {
      df %>% arrange(across(all_of(cols_exist)))
    } else {
      df
    }
  }
  
  
  # ---------------------------------------------------------------------------- #
  # Part 1: This part prepares the Pop data for further analysis by:
  # introducing uniform names
  # adding group columns and id if missing
  # ---------------------------------------------------------------------------- #
  
  Pop        <- as_tibble(Pop)
  names(Pop) <- tolower(names(Pop))
  
  if(!"sex"          %in% names(Pop)) Pop$sex          <- sex
  if(!"country_code" %in% names(Pop)) Pop$country_code <- country_code %||% 1
  if(!"name"         %in% names(Pop)) Pop$name         <- name %||% "country"
  if(!".id"          %in% names(Pop)) {
    
    Pop <- create_groupid(Pop, keys = c("name", "sex", "year", "country_code"))
    
  }
  
 
  # ---------------------------------------------------------------------------- #
  # Part 2: Here we deal with the standard data
  # first case is when the data is not provided by user
  # in this case, it is being pulled from the latest installed wpp package
  # if the data is provided by the user, then we filter corresponding info from it
  # by default we calculate nLx for chosen combination for single years
  # then if user provided data are in 5 or 10-year intervals, we group our nLx accordingly
  # if spacing is strange the warning is thrown
  # ---------------------------------------------------------------------------- #
  
  if(is.null(nLx)) {
    
    installed_wpp <- grep("^wpp\\d{4}$", rownames(installed.packages()), value = TRUE)
    
    if(length(installed_wpp) == 0) stop("No wpp package installed.")
    
    latest_wpp <- sort(installed_wpp, decreasing = TRUE)[1]
    suppressPackageStartupMessages(library(latest_wpp, character.only = TRUE))
    data("mx1dt", package = latest_wpp)
    
    nLx <- mx1dt %>%
      as_tibble() %>%
      select(-mxB) %>%
      conditional_filter("country_code", country_code) %>%
      conditional_filter("name", name) %>%
      conditional_filter("year", year) %>%
      pivot_longer(c(mxM, mxF), 
                   names_to  = "sex", 
                   values_to = "mx") %>%
      mutate(sex = substr(sex, 3, 3)) %>%
      conditional_filter("sex", sex) %>%
      group_by(across(all_of(intersect(c("name", "country_code", "sex", "year"), names(.))))) %>%
      reframe(lt_single_mx(nMx = mx, Age = age)) %>%
      ungroup() %>%
      select(name:year, age = Age, AgeInt, nLx)
    
  } else {
    
    nLx <- as_tibble(nLx) %>%
      conditional_filter("country_code", country_code) %>%
      conditional_filter("name", name) %>%
      conditional_filter("year", year) %>%
      conditional_filter("sex", sex) %>%
      select(any_of(c("name", "country_code", "sex", "year", "age", "nLx")))
    
  }
  
  # can be changed to is_single if needed
  age_diff    <- diff(sort(unique(Pop$age)))
  unique_diff <- unique(na.omit(age_diff)) # NA removed in case of strange OAG coding
  
  if(length(unique_diff) == 1) {
    
    age_spacing <- unique_diff
    
  } else {
    
    stop("Mixed or irregular age spacing: ", paste(unique_diff, collapse = ", "))
    
  }
  
  # --- Group nLx ages if needed --- #
  if(age_spacing %in% c(5, 10)) {
    
    nLx <- nLx %>%
      group_by(across(all_of(intersect(c("name", "country_code", "sex", "year"), names(.))))) %>%
      reframe(
        age = seq(0, max(age), by = age_spacing),
        nLx = groupAges(nLx, Age = age, N = age_spacing),
        .groups = "drop"
      )
  }
  
 
  # ---------------------------------------------------------------------------- #
  # Part 3: Now we have user data and reference data and we can use the OPAG function
  # ---------------------------------------------------------------------------- #
  
  # we conditionally arrange both datasets to ensure that ids of Pop
  # match row by row with nLx. This makes it easier to work with data further
  Pop <- conditional_arrange(Pop, c("name", "country_code", "sex", "year", "age"))
  nLx <- conditional_arrange(nLx, c("name", "country_code", "sex", "year", "age"))
  
  nLx$.id_label <- Pop$.id_label
  nLx$.id       <- Pop$.id
  
  result <- Pop %>% 
    full_join(nLx) %>%
    group_nest(.id, .id_label) %>% 
    mutate(
      results = map(data, ~ {
        Age_vals  <- unique(.x$age)
        Pop_vals  <- .x$pop
        nLx_vals  <- .x$nLx
        
        OPAG(
          Pop               = Pop_vals,
          Age_Pop           = Age_vals,
          nLx               = nLx_vals,
          Age_nLx           = Age_vals,
          Age_fit           = Age_fit,
          AgeInt_fit        = AgeInt_fit,
          Redistribute_from = Redistribute_from,
          OAnew             = OAnew,
          method            = method
        )
      }),
      
      plots = map2(data, results, ~ {
        old <- tibble(pop = .x$pop, 
                      age = .x$age) %>%
          filter(age > Redistribute_from)
        
        new <- tibble(pop = .y$Pop_out, 
                      age = .y$Age_out) %>%
          filter(age > Redistribute_from)
        
        ggplot() + 
          geom_point(aes(x = old$age, y = old$pop)) +
          geom_line(aes(x = new$age,  y = new$pop), color = "red") +
          theme_minimal()
      })
    )
  
  results        <- result$results
  names(results) <- result$.id_label
  figures        <- result$plots
  names(figures) <- names(results)

  return(list(data_out  = results, 
              figures   = figures
  ))
  
}

# here is how to use
# without user defined nLx
results <- odap_opag(
  Pop = Pop,
  nLx = NULL,
  method = "mono"
)

results$data_out
results$figures

#  with user defined nLx
results2 <- odap_opag(
  Pop = Pop,
  nLx = nLx,
  method = "mono"
)

#naturally resulta are the same
results2$data_out
results2$figures




