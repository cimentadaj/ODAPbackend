# ask Tim about "auto" option and ideal cut points in measures,
# and whether all versions of data should be preserved, and 
# whether lifetable plots should compare with pre and post smoothing
# 
# library(DemoTools)
# 
# 
# data_in <- readr::read_csv(system.file("extdata",
#                                        "abridged_data.csv",
#                                        package="ODAPbackend"))
# plot(
# smooth_age_5(data_in$Deaths, data_in$Age, method = "Arriaga",young.tail="Original",old.tail="Original") 
# smooth_age_5(data_in$Exposures, data_in$Age, method = "Arriaga",young.tail="Original",old.tail="Original"))
# 
# 
#  smooth_counts <- function(Value, 
#                            Age, 
#                            method, 
#                            OAG,
#                            ageMin = 10,
#                            ageMax = 65,
#                            lower.tail, 
#                            upper.tail,
#                            constrain){
#    
#  }