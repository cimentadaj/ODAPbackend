# TR: add some roxygen-style documentation to these

# TR: future modifications:
# -[] allow line skips; allow for footnotes in cells below main table
# -[] allow non-standard column names
# -[] automatic integer recoding of character string definitions of Age, 
#      i.e. "1-4", "1 to 4" etc become 1.

read_data <- function(user_file) { 
  

  # now we know the extension and we can proceed with reading the file 
  extension <- extension_check(user_file)
  
  # if extension is not allowed, throw an error
  stopifnot("File extension not supported at this time. Please provide the data in .csv, .xlsx, or .xls format" =length(extension) == 1 )
  
  # for delimited data
  if(extension %in% c("csv","tsv")) {
    # TR: changed to file.path() because the path separator is then 
    # OS dependent
    
    # For read_delim() no need to specify delim, it's apparently detected; I tried
    # , ; \t
    data_in <- read_delim(file.path("data", user_file), show_col_types = FALSE)
    
    
  } else { 
    # can handle both xls and xlsx data. 
    # we can use readxl if we want to hard code format
    # assumes the data is on a first sheet
    # TR: I think read_excel does both formats too and can handle flexible 
    # positioning
    # TR: AgeInt can have NA in final value, in which case,
    # we need to make sure it reads in as integer and not character
    data_in <- read_excel(file.path("data", user_file), sheet = 1)
    
  } 
  
  # calculate empirical nmx
  data_in <- data_in %>% 
    mutate(Mx_emp = Deaths / Exposures)
  
  return(data_in)
}
