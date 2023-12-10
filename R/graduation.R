# # ask Tim about "auto" option and ideal cut points in measures,
# # and whether all versions of data should be preserved, and 
# # whether lifetable plots should compare with pre and post smoothing
# # 
# # library(DemoTools)
# # 
# # 
# # data_in <- readr::read_csv(system.file("extdata",
# #                                        "abridged_data.csv",
# #                                        package="ODAPbackend"))
# plot(
# 
# smooth_age_5(data_in$Deaths, data_in$Age, method = "Arriaga",young.tail="Original",old.tail="Original")
# 
# 
# 
# smooth_age_5(data_in$Exposures, data_in$Age, method = "Arriaga",young.tail="Original",old.tail="Original")
# 
# 
# )
# 
# DemoTools::heapify()
# 
# eval(parse(text = "sum(1:12)"))
# # 
# # 
# #  smooth_counts <- function(Value, 
# #                            Age, 
# #                            method, 
# #                            OAG,
# #                            ageMin = 10,
# #                            ageMax = 65,
# #                            lower.tail, 
# #                            upper.tail,
# #                            constrain){
# #    
# #  }
# 
# Age <- 0:100
# groupAges(pop1m_ind, N = 5)
# 
# 
# Pop  <-c(303583,390782,523903,458546,517996,400630,485606,325423,471481,189710,
#          385442,143205,270890,145105,138078,157444,153035,91566,247160,73115,
#          384222,83551,198555,111347,129851,477510,149272,100814,178465,50684,
#          577167,51878,97788,55544,58011,393200,85048,51131,80336,31246,
#          454698,34864,51810,31146,26618,228718,38504,23616,40836,15589,
#          339158,21349,26997,17590,17513,119763,22704,12336,17635,8485,
#          323263,9535,13906,9063,8294,90459,9817,6376,8884,3773,160609)
# Age  <- 0:70
# # final age group assumed open
# mav(Pop, n = 3, Age = Age)
# 
# 
# 
# 
# 
# 
# 
# 
# dt <- data.frame(pop = pop1m_ind,
#                  age = 0:100)
# 
# dt
# 
# z <- check_heaping_bachi(pop1m_ind, Age = 0:100, 
#                          ageMin = 23, ageMax = 77, method = "pasex", details = TRUE)
# 
# 
# library(tidyverse)
# 
# tibble(
#   pct = z$pct, 
#   digit = z$digits
# ) %>% 
#   mutate(pct0_pct5 = pct[1] / pct [6]) %>% 
#   arrange(desc(pct)) %>% 
#   mutate(prop2 = pct[1] / pct[6])
# 
# # index remake. works
# sum(abs(z$pct - 10)) / 2
# 
# 
# # BachiProp0and5
# (sum(z$pct[c(1, 6)]) - 20) / z$index
# 
# # Max2prop
# (sum(sort(z$pct, decreasing = TRUE)[c(1, 2)]) - 20 ) / z$index
# 
# 
# 
# smooth_age_5(
#   data_in$Deaths,
#   data_in$Age,
#   method     = "Arriaga",
#   young.tail = "Original",
#   old.tail   = "Original"
# )
# 
# 
# 
# ageRatioScore()
# 
# graduate_mono()
# 
# 
# mav(Pop, n = 3, Age = Age)
# 
# 
# groupAges(pop1m_ind, N = 5)
# 
# 
# 
# # --------------------------------------------------------------- #
# options("install.lock"=FALSE)
# Sys.setenv(LANG = "en")
# 
# 
# ?check_heaping_bachi()
# 
# 
# check_heaping_bachi(pop1m_pasex, Age = 0:99, 
#                     ageMin = 23, ageMax = 77, method = "pasex", OAG = FALSE, details = TRUE)
# 
# 
# 
# plot(pop1m_pasex)
# lines(data.table::frollmean(pop1m_pasex, n = 10, fill = NA))
# 
# mean(pop1m_pasex[1:10])
# 
# 
# 
# ?check_heaping_bachi()
# 
# ?check_heaping_whipple()
# 
# 
# # 2.34,replicates SINGAGE males\
# Age <- 0:99
# 
# (w05 <- check_heaping_whipple(pop1m_pasex, Age, 25, 60, digit = c(0, 5)))
# 
# # implements formula from Roger et al. (1981, p. 148)
# (w0 <- check_heaping_whipple(pop1m_pasex, Age, 25, 60, digit = 0))
# (w5 <- check_heaping_whipple(pop1m_pasex, Age, 25, 60, digit = 5))
# 
# check_heaping_whipple(pop1m_pasex, Age, 25, 60, digit = 3)
# 
