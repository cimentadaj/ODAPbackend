# library(DemoTools)
# library(tidyverse)
# library(scales)
# library(readxl)
library(devtools)
load_all()
# source("R/readers.R")
# source("R/checkers.R")
# source("R/lifetables.R")
# source("R/plots.R") # broken function in this one
# 


Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
               247473,223014,172260,149338,127242,105715,79614,53660,
               31021,16805,8000,4000,2000,1000)

Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
            1335,3257,2200,4023,2167,4578,2956,4212,
            2887,2351,1500,900,500,300)

abridged_data <- tibble(Deaths = Deaths, 
                        Exposures = Exposures, 
                        Age = c(0, 1, seq(5, 100, by = 5)),
                        AgeInt = c(diff(Age), NA),
                        Sex = "Male")

write_delim(abridged_data,"data/abridged_data2.csv",";")
write_csv(abridged_data,"inst/extdata/abridged_data.csv") ### here
write_tsv(abridged_data,"data/abridged_data.tsv")
abridged_data
devtools::check()
# this is fed to us from the user
user_file <- "abridged_data.csv" ### 1
user_file <- "abridged_data.tsv" ### 1
user_file <- "abridged_data2.csv" # semicolon separated
user_file <- "abridged_data1.xlsx"
user_file <- "abridged_data2.xls"
user_file <- "abridged_data2.txt" # to check that function in NOT working with this format

data <- read_data("abridged_data.csv") # good
data <- read_data("abridged_data.tsv") # should we allow it?
data <- read_data("abridged_data2.csv")
data <- read_data("abridged_data1.xlsx")
data <- read_data("abridged_data2.xls")
# we expect an error.... but what if it's delimited?? Shall we just allow it?
data <- read_data("abridged_data2.csv")



# task 3: # We need to make predefined lists of valid values for each argument
# think also of what the user should SEE in the menu versus what value we need to 
# pass to the LT function. they don't need to be the same. First we do abridged.

check_data(data)

plot_the_initial_data(data)

check_heaping_general(data)

# Task 4: make a ggplot code snippet showing the incoming rates as a line
# and the outgoing rates with a dashed line in a different color, potentially
# only starting at the extrapFrom age. This function anticipates the output
# of lt_abridged().

# TR: wrap this in calc_lt() 
# Idea: we can detect whether incoming ages are abridged or single. The user should just need to select whether they want abridged or single outgoing ages. Make sense?

# So out wrapper function would be calc_lt(), passing in all arguments.
# we have lt_single2abridged() and lt_abridged2single(), for instance for
load_all()
data1 <- lt_flexible(Deaths = data$Deaths,
                     Exposures = data$Exposures,
                     Age = data$Age,
                     OAnew = 110,  
                     extrapFrom = 80,
                     extrapFit = seq(70,100,by=5),
                     age_out = "single")

OAnew      = 100   # TR element of basic, gets passed in
extrapFrom = 70    # TR element of advanced, gets passed in
extrapFit  = seq(70, 100, by = 5) # TR element of advanced, gets passed in
Mx_emp <- abridged_data$Deaths/ abridged_data$Exposures

#### lt check
data_out <- 
  lt_flexible(Deaths    = data$Deaths, 
              Exposures = data$Exposures,
              Age       = data$Age,
              OAnew     = 100,
              age_out = "single",  
              extrapFrom = 70,
              extrapFit = data$Age[data$Age >= 60], 
              radix     = 1e+05,
              extrapLaw = NULL,
              SRB       = 1.05,
              a0rule    = "ak",
              axmethod  = "un",
              Sex       = "m")
make_figure(data, data_out, 70)
plot_initial_single_sex(data)
# plot check
# a little data "simulation"
data1           <- data
data1$Sex       <- "Female"
data1$Exposures <- data1$Exposures
data1$Deaths    <- data1$Deaths
data <- data %>% 
  full_join(data1) %>%
  mutate(Deaths = ifelse(Sex == "Female", Deaths + rpois(22, lambda = 50), Deaths))

# works
make_figure(data, data_out)
# basic
basic <- 
  tibble(for_us = c("OAnew",
                    "OAG",
                    "sex",
                    "radix"),
         for_users = c("Desired open age group",
                       "Whether or not the last element of nMx is an open age group.",
                       "Sex",
                       "Radix value"),
         default_calue = c("100",
                           "TRUE",
                           "male",
                           "100000"),
         look = c("field with numeric input",
                  "a box to tick",
                  "a box to tick",
                  "field with numeric input"))

# advanced
advanced <- 
  tibble(for_us = c("extrapLaw",
                    "extrapFrom",
                    "extrapFit",
                    "SRB",
                    "a0rule",
                    "axmethod"),
         for_users = c("Parametric mortality law for LT extrapolation",
                       "Age from which to impute extrapolated mortality",
                       "Ages to include in model fitting.",
                       "The sex ratio at birth (boys / girls)",
                       "The rule for modelling a0 value",
                       "The method to model ax values"),
         default_calue = c("Kannisto if < 90, or makeham",
                           "80",
                           "60",
                           "1.05",
                           "ak",
                           "un"),
         look = c("The dropping list with choices",
                  "a field with numeric input",
                  "filed with numeric input",
                  "field with numeric input",
                  "two coice buttons",
                  "two coice buttons"))


write.csv(basic, file = "basic.csv")




devtools::load_all()
 Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
 247473,223014,172260,149338,127242,105715,79614,53660,
 31021,16805,8000,4000,2000,1000)
 
 Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
             1335,3257,2200,4023,2167,4578,2956,4212,
             2887,2351,1500,900,500,300)

Age = c(0, 1, seq(5, 100, by = 5))
 data_out <- 
   lt_flexible(Deaths    = Deaths, 
               Exposures = Exposures,
               Age       = Age,
               OAnew     = 100,
               age_out   = "single",  
               extrapFrom = 80,
               extrapFit = Age[Age >= 60], 
               radix     = 1e+05,
               extrapLaw = NULL,
               SRB       = 1.05,
               a0rule    = "ak",
               axmethod  = "un",
               Sex       = "m")
 plot_lifetable(data_out = data_out)



write.csv(advanced, file = "advanced.csv")


Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
                              247473,223014,172260,149338,127242,105715,79614,53660,
                              31021,16805,8000,4000,2000,1000)
               
Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
                           1335,3257,2200,4023,2167,4578,2956,4212,
                           2887,2351,1500,900,500,300)
               
data <- tibble(Deaths = Deaths, 
                                       Exposures = Exposures, 
                                       Age = c(0, 1, seq(5, 100, by = 5)))




load_all()
library(tidyverse)
library(devtools)
library(DemoTools)
data(pop1m_ind, package = "DemoTools")
Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
               247473,223014,172260,149338,127242,105715,79614,53660,
               31021,16805,8000,4000,2000,1000)

Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
            1335,3257,2200,4023,2167,4578,2956,4212,
            2887,2351,1500,900,500,300)

data_abr_in <- tibble(Deaths = Deaths, 
                        Exposures = Exposures, 
                        Age = c(0, 1, seq(5, 100, by = 5)),
                        AgeInt = c(diff(Age), NA),
                        Sex = "Male")

data1_in <- data.frame(Exposures = pop1m_ind,
                     Age = 0:100)
data5_in <- data.frame(Exposures = groupAges(pop1m_ind, N = 5),
                      Age = seq(0, 100, 5))
data_other1 <- data5_in |> 
  mutate(Age = ifelse(Age > 50, Age - Age %% 10, Age)) |> 
  summarize(Exposures = sum(Exposures), .by = Age)

data_other1 <- data5_in |> 
  mutate(Age = ifelse(Age > 50, Age - Age %% 10, Age)) |> 
  summarize(Exposures = sum(Exposures), .by = Age)

# big tests

fine_methods <-   c("auto", "none", "sprague",
                                  "beers(ord)", "beers(mod)",
                                  "grabill", "pclm", "mono",
                                  "uniform")
rough_methods = c("auto", "none", "Carrier-Farrag",
                 "KKN", "Arriaga", "United Nations",
                 "Strong", "Zigzag")
age_ins <- c("single","5-year","abridged","other")
age_outs <- c("single","5-year","abridged")


beefy_output <- list()
for (ai in age_ins ){
   
  data_in <- switch(ai,
                    "single" = data1_in,
                    "5-year" = data5_in,
                    "abridged" = data_abr_in,
                    "other" = data_other1)
  for (ao in age_outs){
    for (fm in fine_methods){
      for (rm in rough_methods){
        data_out <- try(smooth_flexible(data_in,
                                    variable = "Exposures", 
                                    rough_method = rm,
                                    fine_method = fm, 
                                    constrain_infants = TRUE, 
                                    age_out = ao, 
                                    u5m = .1,
                                    Sex = "t"))
        labeli <- paste(ai, ao, fm, rm, sep = "-")
        beefy_output[[labeli]] <- data_out
        
      }
    }
  }
}
length(beefy_output)
ind <- lapply(beefy_output, class) |> unlist() %>% '=='("try-error")
sum(ind)
dfind <- lapply(beefy_output, is.data.frame) |> unlist()
 beefy_output[!dfind] |> names()

data(pop1m_ind, package = "DemoTools")
data_in <- data.frame(Exposures = pop1m_ind,
                      Age       = 0:100)
Value <- data_in$Deaths
Age <- data_in$Age
function (Value, Age, ageMin = 40, ageMax = max(Age[Age%%5 == 
                                                      0]) - 10) 
{
ageMin <- ageMin - ageMin%%10
VH5 <- groupAges(Value, Age, 5)
A5 <- names2age(VH5)
adj2 <- avg_adj(VH5)
ai <- A5 >= ageMin & A5 <= ageMax
m05 <- suppressWarnings(matrix((VH5/adj2)[ai], nrow = 2))
if (sum(ai)%%2 != 0 | any(is.na(m05))) {
  m05 <- m05[, -ncol(m05)]
}
1/ratx(rowMeans(m05, na.rm = TRUE))
}



# ----------------------------------------------------------------- #

library(tidyverse)
Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
               247473,223014,172260,149338,127242,105715,79614,53660,
               31021,16805,8000,4000,2000,1000)

Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
            1335,3257,2200,4023,2167,4578,2956,4212,
            2887,2351,1500,900,500,300)

sex <- c("f", "m")

Age = c(0, 1, seq(5, 100, by = 5))
data_in <- tibble(Age,Deaths,Exposures)
dt <- rpois(n = 22, lambda = 1000)
d1 <- rpois(n = 22, lambda = 100000)

data_in <- expand_grid(sex, data_in) %>%
  mutate(Deaths = ifelse(sex == "f", dt, Deaths),
         Exposures = ifelse(sex == "f", d1, Exposures)) %>%
  mutate(.id = sex) %>%
  mutate(Sex = sex) %>%
  dplyr::select(-sex)



ex1 <- smooth_flexible(
  data_in,
  variable     = "Exposures",
  rough_method = "auto",
  fine_method  = "none",
  constrain_infants = TRUE,
  age_out = "abridged",
  u5m     = NULL)


ex2 <- smooth_flexible(
  data_in,
  variable     = "Deaths",
  rough_method = "auto",
  fine_method  = "none",
  constrain_infants = TRUE,
  age_out = "abridged",
  u5m     = NULL)

# exposures
ex1$figures[[1]]
ex1$figures[[2]]

# deaths
ex2$figures[[1]]
ex2$figures[[2]]

# here is the smoothed data
new_in <- ex1$data %>% 
  full_join(ex2$data) %>% 
  mutate(sex = .id)

# lifetable
data_out <- lt_flexible(
  data_in = new_in
)

# summary
lt_summary(na.omit(data_out)) # this is simply because data is from my head


z <- lt_plot(data_in,
             data_out,
             extrapFrom = extrapFrom)


# men (same for women)
z$m[[1]]
z$m[[2]]
z$m[[3]]
z$m[[4]]

# plot
z$nMx[[1]][[1]]

# data
z$nMx[[1]][[2]]

library(devtools)
load_all()
library(DemoTools)
library(tidyverse)
dataIn <- read_csv("/home/tim/Desktop/abridged_data.csv") |> 
  ODAPbackend::create_groupid(c())
dataIn <-
  dataIn |> 
  mutate(Age = if_else(Age == 12,10,Age))
lt_flexible(
  data_in = dataIn,
  OAnew = 100,
  age_out = "single",
  extrapFrom = 80,
  extrapFit = seq(60, 100, by = 5),
  extrapLaw = "Kannisto",
  radix = 100000,
  SRB = 1.05,
  a0rule = "Andreev-Kingkade",
  axmethod = "UN (Greville)",
  Sex = "Total"
)


# TODO

#[ ] for lt comparisons, also give numerical deviations for large
#    datasets
#[ ] some scatterplots
#
#
#

require(graphics)
plot(dist ~ speed, data = cars, main = "data(cars)  &  smoothing splines")
cars.spl <- with(cars, smooth.spline(speed, dist))
cars.spl
## This example has duplicate points, so avoid cv = TRUE

lines(cars.spl, col = "blue")
ss10 <- smooth.spline(cars[,"speed"], cars[,"dist"], df = 10)
lines(ss10, lty = 2, col = "red")
legend(5,120,c(paste("default [C.V.] => df =",round(cars.spl$df,1)),
               "s( * , df = 10)"), col = c("blue","red"), lty = 1:2,
       bg = 'bisque')


## Residual (Tukey Anscombe) plot:
plot(residuals(cars.spl) ~ fitted(cars.spl))
abline(h = 0, col = "gray")

## consistency check:
stopifnot(all.equal(cars$dist,
                    fitted(cars.spl) + residuals(cars.spl)))
## The chosen inner knots in original x-scale :
with(cars.spl$fit, min + range * knot[-c(1:3, nk+1 +1:3)]) # == unique(cars$speed)

## Visualize the behavior of  .nknots.smspl()
nKnots <- Vectorize(.nknots.smspl) ; c.. <- adjustcolor("gray20",.5)
curve(nKnots, 1, 250, n=250)
abline(0,1, lty=2, col=c..); text(90,90,"y = x", col=c.., adj=-.25)
abline(h=100,lty=2); abline(v=200, lty=2)

n <- c(1:799, seq(800, 3490, by=10), seq(3500, 10000, by = 50))
plot(n, nKnots(n), type="l", main = "Vectorize(.nknots.smspl) (n)")
abline(0,1, lty=2, col=c..); text(180,180,"y = x", col=c..)
n0 <- c(50, 200, 800, 3200); c0 <- adjustcolor("blue3", .5)
lines(n0, nKnots(n0), type="h", col=c0)
axis(1, at=n0, line=-2, col.ticks=c0, col=NA, col.axis=c0)
axis(4, at=.nknots.smspl(10000), line=-.5, col=c..,col.axis=c.., las=1)

##-- artificial example
y18 <- c(1:3, 5, 4, 7:3, 2*(2:5), rep(10, 4))
xx  <- seq(1, length(y18), length.out = 201)
(s2   <- smooth.spline(y18)) # GCV
(s02  <- smooth.spline(y18, spar = 0.2))
(s02. <- smooth.spline(y18, spar = 0.2, cv = NA))
plot(y18, main = deparse(s2$call), col.main = 2)
lines(s2, col = "gray"); lines(predict(s2, xx), col = 2)
lines(predict(s02, xx), col = 3); mtext(deparse(s02$call), col = 3)

## Specifying 'lambda' instead of usual spar :
(s2. <- smooth.spline(y18, lambda = s2$lambda, tol = s2$tol))



## The following shows the problematic behavior of 'spar' searching:
(s2  <- smooth.spline(y18, control =
                        list(trace = TRUE, tol = 1e-6, low = -1.5)))
(s2m <- smooth.spline(y18, cv = TRUE, control =
                        list(trace = TRUE, tol = 1e-6, low = -1.5)))
## 


data_in <-
structure(list(x = c(1950.77104722793, 1951.77104722793, 1952.77104722793, 
                     1952.77104722793, 1953.77104722793, 1954.77104722793, 1955.77104722793, 
                     1956.77104722793, 1957.77104722793, 1957.77104722793, 1958.77104722793, 
                     1959.77104722793, 1963.5, 1964.5, 1965.5, 1966.5, 1967.5, 1968.5, 
                     1969.5, 1970.5, 1970.5, 1971.5, 1971.5, 1972.5, 1972.5, 1973.5, 
                     1973.5, 1974.5, 1974.5, 1975.5, 1975.5, 1976.5, 1976.5, 1977.5, 
                     1977.5, 1978.5, 1978.5, 1979.5, 1979.5, 1980.5, 1980.5, 1981.5, 
                     1981.5, 1982.5, 1982.5, 1983.5, 1983.5, 1984.5, 1984.5, 1985.5, 
                     1985.5, 1986.5, 1986.5, 1987.5, 1987.5, 1988.5, 1988.5, 1989.5, 
                     1989.5, 1990.5, 1990.5, 1991.5, 1991.5, 1992.5, 1992.5, 1993.5, 
                     1993.5, 1994.5, 1994.5, 1995.5, 1995.5, 1996.5, 1996.5, 1997.5, 
                     1997.5, 1998.5, 1998.5, 1998.5, 1999.5, 1999.5, 1999.5, 2000.5, 
                     2000.5, 2000.5, 2001.5, 2001.5, 2001.5, 2001.5, 2002.5, 2002.5, 
                     2002.5, 2003.5, 2003.5, 2003.5, 2004.5, 2004.5, 2005.5, 2005.5, 
                     2006.5, 2006.5, 2007.5, 2007.5, 2008.5, 2008.5, 2009.5, 2009.5, 
                     2010.5, 2010.5, 2011.5, 2012.5, 2013.5, 2014.5, 2015.5, 2016.5, 
                     2017.5), y = c(5.28312492370605, 5.4010272026062, 5.55507040023804, 
                                    5.52694797515869, 5.65941572189331, 5.81246614456177, 5.95277643203735, 
                                    6.07998991012573, 6.20043277740479, 6.23209381103516, 6.4145884513855, 
                                    6.44994592666626, 5.77428722381592, 6.09462738037109, 6.31580305099487, 
                                    5.68929624557495, 6.34508848190308, 5.67744398117065, 5.6477165222168, 
                                    5.12978315353394, 4.83979654312134, 5.40941429138184, 4.93997049331665, 
                                    5.06586456298828, 4.68799591064453, 4.31546640396118, 4.18193292617798, 
                                    3.75800633430481, 3.82137632369995, 3.17197370529175, 3.41054058074951, 
                                    3.08278703689575, 3.04342699050903, 3.01940298080444, 2.9705445766449, 
                                    2.96306347846985, 2.88018417358398, 2.72204685211182, 2.72689270973206, 
                                    3.43744516372681, 2.88990616798401, 3.12944483757019, 3.18246674537659, 
                                    3.20358324050903, 3.0853967666626, 3.38533687591553, 3.18455958366394, 
                                    3.14047956466675, 3.08752226829529, 3.15941309928894, 3.09168982505798, 
                                    3.22912931442261, 2.95333743095398, 3.05898070335388, 2.91993451118469, 
                                    3.29801154136658, 2.93581032752991, 3.18667984008789, 2.92741537094116, 
                                    3.10476756095886, 2.86983323097229, 3.31816387176514, 2.87090802192688, 
                                    3.57006907463074, 3.20188307762146, 3.38520860671997, 3.1142041683197, 
                                    3.05472731590271, 2.88588929176331, 2.85394668579102, 2.69696426391602, 
                                    2.71265292167664, 2.49935364723206, 2.55814361572266, 2.37988924980164, 
                                    2.35105299949646, 2.32249999046326, 2.21962690353394, 2.46929597854614, 
                                    2.43600010871887, 2.6223669052124, 2.31005716323853, 2.33249998092651, 
                                    2.408358335495, 2.39718627929688, 2.38599991798401, 3, 2.51987075805664, 
                                    2.1131649017334, 2.13849997520447, 2.04657125473022, 2.06265759468079, 
                                    2.10050010681152, 1.98469293117523, 2.09120869636536, 2.2427921295166, 
                                    1.97930490970612, 2.15000796318054, 2.06816005706787, 2.18898129463196, 
                                    1.77942955493927, 1.99617087841034, 1.89081299304962, 2.0644690990448, 
                                    1.85119438171387, 2.03594541549683, 1.83351850509644, 2.02167296409607, 
                                    1.89562964439392, 1.89205574989319, 1.85763072967529, 1.68259692192078, 
                                    1.69228148460388, 1.56845271587372, 1.37076985836029)), 
          row.names = c(NA, -115L), class = c("tbl_df", "tbl", "data.frame")) |> 
  mutate(w=1,.id="all")

load_all()

smooth1d(data_in)


 library(readr)
 library(dplyr)
 # single age data
 fpath <- system.file("extdata", 
 "single_hmd_spain.csv", 
 package = "ODAPbackend")
 data_in <- read_csv(fpath) |>
   dplyr::select(-1) |>
   filter(.id == 1) |> 
   mutate(Mx_emp = Deaths / Exposures)
 
 data_out <-
   lt_flexible_chunk(data_in,
                     OAnew     = 100,
                    age_out   = "abridged",
                    extrapFrom = 80,
                    # extrapFit = Age[Age >= 60],
                    radix     = 1e+05,
                     extrapLaw = NULL,
                     SRB       = 1.05,
                    a0rule    = "ak",
                    axmethod  = "un")
 
 library(devtools)
 load_all()
 library(readr)
 library(tidyverse)
 library(dplyr)
 # single age data
 fpath <- system.file("extdata", 
 "single_hmd_spain.csv", 
 package = "ODAPbackend")
 data_in <- read_csv(fpath) |>
   dplyr::select(-1) |> 
   mutate(Mx_emp = Deaths / Exposures)
 load_all()
 data_out <- lt_flexible(
   data_in,
   OAnew      = 100,
   age_out    = "single",
   extrapFrom = 80,
   extrapFit  = NULL,  # Default NULL, computed later
   extrapLaw  = NULL,
  radix      = 1e+05,
   SRB        = 1.05,
   a0rule     = "ak",
   axmethod   = "un"
 )
 
 library(readr)
 library(dplyr)
 # single age data
 fpath <- system.file("extdata", 
 "single_hmd_spain.csv", 
 package = "ODAPbackend")
data_in <- read_csv(fpath) |>
  dplyr::select(-1)
data_in$.id |> table()
data_out <- lt_flexible(
  data_in,
  OAnew      = 100,
  age_out    = "single",
  extrapFrom = 80,
  extrapFit  = 80:100, 
  extrapLaw  = "Kannisto",
  radix      = 1e5,
  SRB        = 1.05,
  a0rule     = "ak",
  axmethod   = "un"
)

data_in2 <- data_in |> dplyr::filter(Sex == "Total")|> 
  mutate(Mx_emp = Deaths / Exposures)

lt_flexible_chunk(data_in.    = data_in2, 
                  Sex        = "t",  
                  OAnew      = 100,  
                  extrapFrom = 80,
                  extrapFit  = 80:100, 
                  radix      = 1e5,
                  extrapLaw  = "Kannisto",
                  SRB        = 1.05,
                  a0rule     = "ak",
                  axmethod   = "un",
                  age_out    = "single")

 fpath <- system.file("extdata", 
 "single_hmd_spain.csv", 
 package = "ODAPbackend")
 data_in <- read_csv(fpath, show_col_types = FALSE) |>
   dplyr::select(-1) |>
   # This step happens in lt_flexible...
   mutate(Mx_emp = Deaths / Exposures,
   Sex = substr(Sex,1,1) |> tolower())
 
 data_out <-
   lt_flexible(data_in,
               OAnew     = 100,
               age_out   = "abridged",
               extrapFrom = 80,
                    # extrapFit = Age[Age >= 60],
               radix     = 1e+05,
               extrapLaw = NULL,
               SRB       = 1.05,
               a0rule    = "ak",
               axmethod  = "un")
 data_out <- lt_flexible(
   data_in,
   OAnew      = 100,
   age_out    = "single",
   extrapFrom = 80,
   extrapFit  = NULL,  # Default NULL, computed later
   extrapLaw  = NULL,
  radix      = 1e+05,
   SRB        = 1.05,
   a0rule     = "Andreev-Kingkade",
   axmethod   = "UN (Greville)"
 )
 
 ex1 <-  lt_plot(
   data_in = data_in,
   data_out = data_out,
   extrapFrom = 80)
 
 
 IN <- readr::read_csv("~/Downloads/deaths_pop_Brazil.csv") 
 readr::read_csv("~/Downloads/test.csv") |> 
   lt_flexible()  |> 
   lt_summary()
 
 lt_summary(lt_flexible(readr::read_csv("~/Downloads/test.csv"))$data_out) 
devtools::load_all()
 library(tidyverse)
 original <- readr::read_csv("deaths_pop_Brazil.csv") %>% ODAPbackend::create_groupid(c("Year", "Sex", "UF"))
 
 smoothed <- readr::read_csv("combined_data.csv")
 
 orig <- lt_flexible(original)
 smooth <- lt_flexible(smoothed)
 smoothabr <- lt_flexible(smoothed,age_out = "abridged")

 
library(readr)
library(dplyr)
library(future)
# single age data
fpath <- system.file("extdata", 
"single_hmd_spain.csv.gz", 
package = "ODAPbackend")
data_in <- read_csv(fpath) |>
 dplyr::select(-1)

data_out <- lt_flexible(
 data_in,
 OAnew      = 100,
 age_out    = "single",
 extrapFrom = 80,
 extrapFit  = NULL,  # Default NULL, computed later
 extrapLaw  = NULL,
 radix      = 1e+05,
 SRB        = 1.05,
 a0rule     = "Andreev-Kingkade",
 axmethod   = "UN (Greville)"
)


load_all()
 library(readr)
 library(dplyr)
 # single age data
 fpath <- system.file("extdata", 
 "single_hmd_spain.csv.gz", 
 package = "ODAPbackend")
 data_in <- read_csv(fpath) |>
   dplyr::select(-1)
 
 data_out <- lt_flexible(
   data_in,
   OAnew      = 100,
   age_out    = "single",
   extrapFrom = 80,
   extrapFit  = NULL,  # Default NULL, computed later
   extrapLaw  = NULL,
   radix      = 1e+05,
   SRB        = 1.05,
   a0rule     = "Andreev-Kingkade",
   axmethod   = "UN (Greville)"
 )
 
 lt_check(data_out)
 