# 
# 
# capture_args <- function(){
#   # parents <- check_parents()
#   call_up <-new.env(hash = TRUE, parent = parent.frame(n=1), size = 29L)
#   # call_up <-  parent.env(sys.nframe()-1)
#   args <- mget(names(formals(envir = call_up)), envir = call_up, inherits=TRUE)
#   args <- args[names(args) != "..."]
#   extras <- list(...)
#   args <- c(args, extras)
#   args
# 
# }
# f <- function(x=1,y=2,...){
#   out <- x+y
#   args <- capture_args()
#   return(list(out, args))
# }
# f()
# given_args <- mget(names(formals()),sys.frame(sys.nframe()))
# 
# get_args <- function()
# {
#   cl <- sys.call(-1)
#   f <- get(as.character(cl[[1]]), mode="function", sys.frame(-2))
#   cl <- match.call(definition=f, call=cl)
#   as.list(cl)[-1]
# }
# get_args <- function () {
#   args <- match.call(
#     def = sys.function( 1 ),
#     call = sys.call(1))
#   args
# }
# 
# f <- function(x=1,y=2,...){
#   out <- x+y
#   args <- capture_args()
#   return(list(out, args))
# }
# f()
# check_parents <- function(){
#   sys.parents()
# }
# g <- function(x=1,y=2,...){
#   check_parents()
# }
# g()
# 
# library(tidyverse)
# library(HMDHFDplus)
# library(janitor)
# library(scales)
# E <- readHMDweb("ESP", "Exposures_1x1", username = Sys.getenv("us"), password = Sys.getenv("pw"))
# B <- readHMDweb("ESP", "Births", username = Sys.getenv("us"), password = Sys.getenv("pw"))
# D <- readHMDweb("ESP", "Deaths_1x1", username = Sys.getenv("us"), password = Sys.getenv("pw"))
# 
# 
# EE <- E |> 
#   filter(Year == 2019) |> 
#   summarize(E = sum(Total)) |> 
#   pull(E)
# BB <- B |> 
#   filter(Year == 2019) |> 
#   summarize(B = sum(Total))|> 
#   pull(B)
# DD <- D |> 
#   filter(Year == 2019) |> 
#   summarize(D = sum(Total)) |> 
#   pull(D)
# 
# 
# cdr = DD / EE
# D <- 
#   D |> 
#   clean_names() |> 
#   filter(year == 2019) |> 
#   select(-open_interval) |> 
#   pivot_longer(female:total, names_to = "sex", values_to = "deaths")
# E <- 
#   E |> 
#   clean_names() |> 
#   filter(year == 2019) |> 
#   select(-open_interval) |> 
#   pivot_longer(female:total, names_to = "sex", values_to = "exposure")
# 
# dat <- left_join(D,E,by=join_by(year,sex,age)) |> 
#   mutate(mx = deaths / exposure) 
#   
# 
# dat |> 
#   filter(sex == "total") |> 
#   ggplot(aes(x = age, y = mx)) +
#   geom_line(linewidth = 1) +
#   #scale_y_log10() +
#   theme_minimal() +
#   labs(x = "Edad") +
#   geom_hline(yintercept = cdr, color = "red", linewidth = 1)  +
#   theme(text = element_text(size = 14, face = "bold"))
# 
# 
# dat |> 
#   filter(sex == "total") |> 
#   ggplot(aes(x = age, y = exposure)) +
#   geom_col(width=1) +
#   #scale_y_log10() +
#   theme_minimal() +
#   labs(x = "Edad",
#        y = "Exposición") +
#   theme(text = element_text(size = 14, face = "bold"))
# 
# 
# dat |> 
#   mutate(mx = if_else(mx == 0, NA, mx)) |> 
#   ggplot(aes(x = age, y = mx, color = sex)) +
#   geom_line(linewidth = 1) +
#   scale_y_log10() +
#   theme_minimal() +
#   labs(x = "Edad") +
#   #geom_hline(yintercept = cdr, color = "red", linewidth = 1)  +
#   theme(text = element_text(size = 14, face = "bold"))
# 
# 
# dat |> 
#   group_by(sex) |> 
#   summarize(cdr = sum(deaths) / sum(exposure))
# 
# 
# dat |> 
#   filter(sex != "total") |> 
#   select(age, sex, mx) |> 
#   pivot_wider(names_from = sex, values_from = mx) |> 
#   mutate(mxd = male - female) |> 
#   ggplot(aes(x = age, y = mxd)) +
#   geom_line()
# dir("C:/Users/Tim/Desktop/")
# 
# stand <- read_csv("C:/Users/Tim/Desktop/MAICS ejercicios - standards.csv")
# 
# stand |> 
#   pivot_longer(-edad, names_to = "standard",values_to = "proportion") |> 
#   mutate(porcentaje = 100* proportion) |> 
#   ggplot(aes(x= edad, y = porcentaje, color = standard)) +
#   geom_line(linewidth = 1) +
#   ylim(0,10) +
#   theme_minimal()+
#   theme(text = element_text(size = 14, face = "bold"))
# 
# eust <- 
# c(0.01,0.04,0.055,0.055,0.055,0.06
# ,0.06,0.065,0.07,0.07,0.07,0.07,0.065
# ,0.06,0.055,0.05,0.04,0.025,0.015,0.008,0.002)
# age <- c(0,1,seq(5,95,by=5))
# 
# st <- tibble(age = age, estandar = eust)
# library(DemoTools)
# dat |> 
#   mutate(age = case_when(age %in% 1:4 ~ 1,
#                          age >95 ~ 95,
#                          TRUE~ age - age %% 5 )) |> 
#   group_by(age, sex) |> 
#   summarize(deaths = sum(deaths),
#             exposure = sum(exposure)) |> 
#   left_join(st, by = join_by(age)) |> 
#   group_by(sex) |> 
#   summarize(CDR = 1000*sum(deaths) / sum(exposure),
#             ASDR =  1000*sum((deaths / exposure) * estandar))



