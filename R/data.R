# Commented out: for now all data moved to inst/extdata
# They can be read in like so:
# fpath <- system.file("extdata", "abridged_data.csv.gz", package="ODAPbackend")
# read_csv(fpath)

# #' dat_heap_smooth
# #' @description
# #' \itemize{
# #'   \item Age dbl, Single ages 0-110+
# #'   \item Deaths double, Death counts for a single year heapifyed
# #'   \item Exposures double, population exposed to risk heapifyed
# #'   \item Population double, population counts for a single year heapifyed

# #' }.
# #' @docType data
# #' @format
# #'  a data.frame with four columns and 111 rows
# #' @source
# #'   Human Mortality Database
# "dat_heap_smooth"
#
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

# library(HMDHFDplus)
# D <- readHMDweb("DEUTNP","Deaths_1x1", username = Sys.getenv("us"), password = Sys.getenv("pw"))
# E <- readHMDweb("DEUTNP","Exposures_1x1", username = Sys.getenv("us"), password = Sys.getenv("pw"))
# 
# D <-
#   D |> 
#   filter(Year == 2010) |> 
#   select(Age, Deaths = Total) |> 
#   mutate(Age = ifelse(Age > 100, 100, Age)) |> 
#   summarize(Deaths = sum(Deaths), .by = Age) |> 
#   mutate(Deaths = heapify(Value = Deaths, Age = Age, ageMax = 90, p0 = 1.15, p5 = 1.1))
# 
# E <-
#   E |> 
#   filter(Year == 2010) |> 
#   select(Age, Exposures = Total) |> 
#   mutate(Age = ifelse(Age > 100, 100, Age)) |> 
#   summarize(Exposures = sum(Exposures), .by = Age) |> 
#   mutate(heapify(Value = Exposures, Age = Age, ageMax = 90, p0 = 1.05, p5 = 1))
# 
# dat_heap_smooth <- left_join(D,E,by = join_by(Age))
# 
# write_csv(dat_heap_smooth, file = "inst/extdata/dat_heap_smooth.csv")
# 
# data_in <- readr::read_csv(system.file("extdata",
#                                        "dat_heap_smooth.csv",
#                                        package="ODAPbackend"))

