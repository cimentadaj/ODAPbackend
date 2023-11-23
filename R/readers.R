
#' read_data
#' @description Read the with supported file extension into the program. The file should contain at minimum 3 columns "Deaths", "Exposures","Age" named this way or positioned in a corresponding order.
#' @param user_file character. File name with corresponding extension e.g. `data.csv`.
#' @param skip numeric. Number of rows to skip before reading the data. Defaults to `0`.
#' @return A tibble with with 5 numeric columns: Deaths, Exposures, Age, AgeInt, Mx_emp.
#' @importFrom dplyr mutate select
#' @importFrom readr read_delim parse_number
#' @importFrom magrittr %>% 
#' @importFrom readxl read_excel
#' @importFrom stringr str_detect 
#' @importFrom DemoTools is_age_coherent is_age_sequential is_age_redundant
#' @importFrom purrr map set_names
#' @importFrom tidyselect matches
#' @export
#' @examples
#' \dontrun{
#' read_data(
#'     user_file = "data.csv",
#'     skip = 2)
#' }

read_data <- function(user_file, skip = 0) {
  
  # now we know the extension and we can proceed with reading the file 
  extension <- extension_check(user_file)
  
  # if extension is not allowed, throw an error
  # stopifnot("File extension not supported at this time. Please provide the data in .csv, .xlsx, or .xls format" = length(extension) == 1 )
  
  # for delimited data
  if(extension %in% c("csv", "tsv","txt")) {
    # TR: changed to file.path() because the path separator is then 
    # OS dependent
    
    # For read_delim() no need to specify delim, it's apparently detected; I tried
    # , ; \t
    data_in <- 
      read_delim(file.path("inst/extdata", user_file), show_col_types = FALSE, skip = skip)
    
    
  } else {
    # can handle both xls and xlsx data. 
    # we can use readxl if we want to hard code format
    # assumes the data is on a first sheet
    # TR: I think read_excel does both formats too and can handle flexible 
    # positioning
    # TR: AgeInt can have NA in final value, in which case,
    # we need to make sure it reads in as integer and not character
    data_in <- read_excel(file.path("inst/extdata", user_file), sheet = 1, skip = skip)
  
  }

  # if age is not numeric convert to numeric
  if(is.character(data_in$Age)) { 
    
    data_in$Age <- parse_number(data_in$Age)
    
  }
  
  # if AgeInt is missing, create AgeInt
  if(!"AgeInt" %in% names(data_in)) {
    
    data_in$AgeInt <- c(diff(data_in$Age), NA)
    
  }
  
  # here we just guarantee that the 5 columns come in a given order and with a given name
  data_in <- data_in %>% 
    dplyr::select(matches("Deaths"), matches("Exposures"), matches("Age$"), matches("AgeInt$"), matches("Sex$")) %>% 
    set_names(c("Deaths", "Exposures", "Age", "AgeInt", "Sex", names(.)[-c(1:5)]))
  
  # calculate empirical nmx
  data_in <- data_in %>%
    mutate(Mx_emp = Deaths / Exposures)
  # 
  return(data_in)
}

