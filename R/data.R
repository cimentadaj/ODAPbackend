# Commented out: for now all data moved to inst/extdata
# They can be read in like so:
# fpath <- system.file("extdata", "abridged_data.csv", package="ODAPbackend")
# read_csv(fpath)


# #' abridged_data
# #' @description
# #' \itemize{
# #'   \item Deaths double, Death counts for a single year and sex
# #'   \item Exposures double, population exposed to risk
# #'   \item Age integer, lower bound of standard abridged ages 0-110+
# #'   \item AgeInt integer, width of age intervals, where NA is used for the open age interval.
# #'   \item Sex character, sex of the corresponding population currently coded as "Male"

# #' }.
# #' @docType data
# #' @format
# #'  a data.frame with five columns and 22 rows
# #' @source
# #'   Human Mortality Database
# "abridged_data"
# 
# #' abridged_data2
# #' @description
# #' \itemize{
# #'   \item Deaths double, Death counts for a single year and sex
# #'   \item Exposures double, population exposed to risk
# #'   \item Age integer, lower bound of standard abridged ages 0-110+
# #'   \item AgeInt integer, width of age intervals, where NA is used for the open age interval.
# #' }.
# #' 
# #' @docType data
# #' @format
# #'  a data.frame with four columns and 22 rows
# #' @source
# #'   Human Mortality Database
# "abridged_data2"