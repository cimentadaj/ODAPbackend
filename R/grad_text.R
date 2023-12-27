
# if(is_single()) { 
#   
#   
# } else { 
#     
#   bachi <- check_heaping_bachi(pop1m_ind, Age = 0:100, 
#                            ageMin = 23, ageMax = 77, method = "pasex", details = TRUE)
#   
# }



# bachi <- check_heaping_bachi(pop1m_ind, 
#                              Age     = 0:100, 
#                              ageMin  = 23, 
#                              ageMax  = 77, 
#                              method  = "pasex", 
#                              details = TRUE)
# index <- bachi$index
# pct   <- bachi$pct

# BachiProp0and5
# Proportion of heaping concentrated in digits 0 and 5
# .prop0and5 <- (sum(pct[c(1, 6)]) - 20) / index

# Max2prop
# Proportion of heping concentrated in the most prefered 2 digits
# .maxprop2 <- (sum(sort(pct, decreasing = TRUE)[c(1, 2)]) - 20 ) / index
  
# .ind <- c(rep(.prop0and5,2),rep(.maxprop2,2), Inf)
# We dont have years of education. So I always choose the maximum mav of the two avalable
# table is not ok, sequence of if elses is better in my opinion
# values <- 
#   n <- 
#   tibble(
#   min_bachi = c(4, 2, 0.75, 0,    8),
#   max_bachi = c(8, 4, 2,    0.75, 30),
#   second_index = c(0.65, 0.6, 0.7, 0.55,101),
#   ind = .ind,
#   mav_val_y = c(10, 6, 4, 2, 10),
#   mav_val_n = c(6,  4, 2, 1, 10)) %>%
#     filter(min_bachi < 29 & max_bachi > 29) %>%  # here
#     mutate(my_n = if_else(ind > second_index, mav_val_y, mav_val_n)) %>% 
#     pull(my_n)
# 
#   mav_part(n = n)
# 
# our_values <- values %>% 
#   rowwise() %>% 
#   mutate(bachi_ind_check = ifelse(between(7.99, min_bachi, max_bachi), 1, 0)) %>%
#   filter(bachi_ind_check == 1) %>% 
#   select_if(!is.na(.)) %>% 
#   dplyr::select(-c(1:2), -bachi_ind_check)
# 
# if("prop0and5" %in% names(our_values)) { 
#   
#   if(prop0and5 > our_values[ ,1, drop = TRUE]) { 
#     
#     pop1m_ind <- mav(Value = pop1m_ind, Age = 0:(length(pop1m_ind) - 1), n = our_values$mav_val_y, tails = TRUE)
#     
#   } else { 
#       
#     pop1m_ind <- mav(Value = pop1m_ind, Age = 0:(length(pop1m_ind) - 1), n = our_values$mav_val_n, tails = TRUE)
#     
#     }
#   
# } else { 
#     
#   if(maxprop2 > our_values[ ,1, drop = TRUE]) {
#     
#     pop1m_ind <- mav(Value = pop1m_ind, Age = 0:(length(pop1m_ind) - 1), n = our_values$mav_val_y, tails = TRUE)
#     
#   } else {
#       
#     pop1m_ind <- mav(Value = pop1m_ind, Age = 0:(length(pop1m_ind) - 1), n = our_values$mav_val_n, tails = TRUE)
#     
#     }
#   
#   }
# 
# 
# 
# 
# 
# mav_part <- function(n) { 
#   
#   mav(Value = pop1m_ind, Age = 0:(length(pop1m_ind) - 1), n = n, tails = TRUE)
#   
#   }
# 
# 
# 
# if(between(index, 4, (8 - 0.00001)))  { 
#   
#   if(prop0and5 > 0.65) { 
#     
#     pop1m_ind <- mav_part(n = 10)
#     
#   } else { 
#     
#     pop1m_ind <- mav_part(n = 6)
#     
#     }
#   
# } else if(between(index, 2, 4 - 0.00001)) { 
#     
#   if(prop0and5 > 0.6) { 
#     
#     pop1m_ind <- mav_part(n = 6)
#     
#   } else { 
#       
#     pop1m_ind <- mav_part(n = 6)
# 
#     }
#   
# } else if(between(index, 0.75, 2 - 0.00001)) { 
#     
#   if(maxprop2 > 0.7) { 
#     
#     pop1m_ind <- mav_part(n = 4)
#     
#   } else { 
#     
#     pop1m_ind <- mav_part(n = 2)
# 
#     }
#   
# } else if(between(index, 0, 0.75 - 0.00001)) { 
#   
#   if(maxprop2 > 0.55) { 
#     
#     pop1m_ind <- mav_part(n = 2)
#     
#   } else { 
#     
#     pop1m_ind <- pop1m_ind
#     
#   }
#   
# } else if(between(index, 8, 30 - 0.00001)) { 
#   
#   pop1m_ind <- mav_part(n = 10)
#   
# } else if(index >= 30) { 
#   
#   # group data in 5 years
#   data_5_y <- groupAges(pop1m_ind, N = 5)
#   
#   # calculate the age ratio score before smoothing
#   age_rat_score_1 <- ageRatioScore(Value = data_5_y, Age = names(data_5_y))
#   
#   # smooth with mva = 2
#   dt5_mav <- mav(Value = data_5_y, Age = names(data_5_y), n = 2, tails = TRUE)
#   
#   # calculate the age ratio score after smoothing
#   age_rat_score_2 <- ageRatioScore(Value = !is.na(dt5_mav), Age = names(dt5_mav))
#   
#   if(age_rat_score_1 < 4) { 
#     
#     data_5_y <- data_5_y
# 
#   } 
#   
#   if(age_rat_score_1 < 4 & age_rat_score_2 < 4) { 
#     
#     data_5_y <- mav(Value = data_5_y, Age = names(data_5_y), n = 2, tails = TRUE)
# 
#   } else { 
#       
#     data_5_y <- mav(Value = data_5_y, Age = names(data_5_y), n = 4, tails = TRUE)
#     
#     dt_fn <- structure(data_5_y, 
#                        .Names = names(data_5_y))
#     
#     # gradute to single ages
#     final <- graduate_mono(dt_fn, OAG = TRUE)
#     }
# }
# 
# # abridged or single ages back
# 
# 
# if(is_single() & index <= 30) { 
#   
#   
# } else if(is_single() & index > 30) {
#     
#   data_5_y <- groupAges(pop1m_ind, N = 5)
#   
#   # calculate the age ratio score before smoothing
#   age_rat_score_1 <- ageRatioScore(Value = data_5_y, Age = names(data_5_y))
#   
#   # smooth with mva = 2
#   dt5_mav <- mav(Value = data_5_y, Age = names(data_5_y), n = 2, tails = TRUE)
#   
#   # calculate the age ratio score after smoothing
#   age_rat_score_2 <- ageRatioScore(Value = !is.na(dt5_mav), Age = names(dt5_mav))
#   
# 
# } else { 
#   
#   # calculate the age ratio score before smoothing
#   age_rat_score_1 <- ageRatioScore(Value = data_5_y, Age = names(data_5_y))
#   
#   # smooth with mva = 2
#   dt5_mav <- mav(Value = data_5_y, Age = names(data_5_y), n = 2, tails = TRUE)
#   
#   # calculate the age ratio score after smoothing
#   age_rat_score_2 <- ageRatioScore(Value = !is.na(dt5_mav), Age = names(dt5_mav))
#   
#   
#   
#   }
# 
#   
#   # group data in 5 years
#   data_5_y <- groupAges(pop1m_ind, N = 5)
#   
#   # calculate the age ratio score before smoothing
#   age_rat_score_1 <- ageRatioScore(Value = data_5_y, Age = names(data_5_y))
#   
#   # smooth with mva = 2
#   dt5_mav <- mav(Value = data_5_y, Age = names(data_5_y), n = 2, tails = TRUE)
#   
#   # calculate the age ratio score after smoothing
#   age_rat_score_2 <- ageRatioScore(Value = dt5_mav, Age = names(dt5_mav))
#   
#   tibble(age_rat = (4),
#          age_rat2 = c(4, 4.1),
#          mav = c(1, 2)) %>% 
#     filter(4 < age_rat_score_1) %>% 
#     filter(age_rat2 < age_rat_score_2)
#   
#  tibble(un = 4,
#         sm = 4) %>% 
#    mutate(n = case_when(
#      4 > age_rat_score_1 ~ 1,
#      4 > age_rat_score_2 ~ 2,
#      TRUE ~ 4
#    )) %>% 
#    pull(n)
#             
#      
#  
#  data_5_y <- mav(Value = data_5_y, Age = names(data_5_y), n = n, tails = TRUE)
#  
#  dt_fn <- structure(data_5_y, 
#                     .Names = names(data_5_y))
#  
#  # gradute to single ages
#  final <- graduate_mono(dt_fn, OAG = TRUE)
#   
# 
# # kids
#  
#  tibble(un = 4,
#         sm = 4) %>% 
#    mutate(n = case_when(
#      4 > age_rat_score_1 ~ 1,
#      4 > age_rat_score_2 ~ 1,
#      TRUE ~ 2
#    )) %>% 
#    pull(n)
#  
#  
#  # -------------------------------------------------------------------- #
#  
#  
#  # We dont have years of education. So I always choose the maximum mav of the two available
#  # calulate n
#  n <- tibble(
#    min_bachi    = c(4, 2, 0.75, 0,    8),
#    max_bachi    = c(8, 4, 2,    0.75, 30),
#    second_index = c(0.65, 0.6, 0.7, 0.55, 101),
#    ind          = indx,
#    mav_val_y    = c(10, 6, 4, 2, 10),
#    mav_val_n    = c(6,  4, 2, 1, 10)) %>%
#    filter(min_bachi < 29 & max_bachi > 29) %>%  # here
#    mutate(my_n = if_else(ind > second_index, mav_val_y, mav_val_n)) %>% 
#    pull(my_n)
#  
#  # indexes combined for table substractionm, last is for n = 10
#  indx <- c(rep(prp0and5, 2),rep(mxprop2, 2), Inf)
#  
#  
#  # smooth data with mav = 2 for both kids and adults
#  dat5_mav_kids   <- mav(Value = kids$Pop, 
#                         Age   = kids$Age, 
#                         n     = 2, 
#                         tails = TRUE)
#  
# 
# data_full <- c(data_kids[-length(data_kids)], data_adults)
# 
# data_out <- tibble(!!variable := data_full,
#                    Age = as.numeric(names(data_full)))