
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

# TR: add some roxygen-style documentation to these
# TR: future modifications:
# -[] allow line skips; allow for footnotes in cells below main table
# -[] allow non-standard column names
# -[] automatic integer recoding of character string definitions of Age, 
#      i.e. "1-4", "1 to 4" etc become 1.

# TODO: the statement data_in$Age <- parse_number(data_in$Age)
# appears twice, after each way of reading in the data; why not
# just do it once after reading in?
# TODO: Can we also check to see if AgeInt is specified? If not, then can we
# create it? This would be down by where we create Mx_emp.

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
      read_delim(file.path("data", user_file), show_col_types = FALSE, skip = skip)
    
    if(is.character(data_in$Age)) { 
      
      data_in$Age <- parse_number(data_in$Age)
      
    }
    
    data_in <- data_in %>% 
      dplyr::select(matches("Deaths"), matches("Exposures"), matches("Age$"), matches("AgeInt$")) %>% 
      set_names(c("Deaths", "Exposures", "Age", names(.)[-c(1:3)]))
    
  } else { 
    # can handle both xls and xlsx data. 
    # we can use readxl if we want to hard code format
    # assumes the data is on a first sheet
    # TR: I think read_excel does both formats too and can handle flexible 
    # positioning
    # TR: AgeInt can have NA in final value, in which case,
    # we need to make sure it reads in as integer and not character
    data_in <- read_excel(file.path("data", user_file), sheet = 1, skip = skip)
    
    if(is.character(data_in$Age)) { 
      
      data_in$Age <- parse_number(data_in$Age)
      
    }
    
    data_in <- data_in %>%
      dplyr::select(matches("Deaths"), matches("Exposures"), matches("Age$"), matches("AgeInt$")) %>% 
      set_names(c("Deaths", "Exposures", "Age", names(.)[-c(1:3)]))
    
  } 

  
  # calculate empirical nmx
  data_in <- data_in %>%
    mutate(Mx_emp = Deaths / Exposures)
  # 
  return(data_in)
}

# we can manually check for the delimiter if we want in the following manner
# I think that at this point we should be rigid in terms of what kind of files and delimiters are allowed

# if(str_detect(readLines("data/abridged_data.tsv", n=1), "\t")) { 
#   
#   read_delim("data/abridged_data.tsv")
#   
# } else if(str_detect(readLines("data/abridged_data.csv", n=1), ",")) { 
#   
#   read_csv("data/abridged_data.csv")
#   
#   
#   
# } else if(str_detect(readLines("data/abridged_data.csv", n=1), ";")) { 
#   
#   read_csv1("data/abridged_data.csv")
#   
# } else { 
#   
#   data_in <- read_excel(file.path("data", user_file), sheet = 1)
#   
# }