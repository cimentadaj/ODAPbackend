library(DemoTools)
library(tidyverse)
library(wpp2024)
library(ODAPbackend)


# check_heapping_general
# add .id behaviour


# Examples for odap_opag

# ---------------------------------------------------------------------------- #
# We start with preparing the model data for testing
# ---------------------------------------------------------------------------- #
# prepare mx data as a dataframe
Pop <- pop1m_ind %>% 
  as.data.frame() %>%
  rownames_to_column() %>% 
  set_names(c("age", "Pop")) %>%
  mutate(sex = "M", 
         year = 1971, 
         name = "India") %>% 
  as_tibble() %>% 
  mutate(age = as.numeric(age) - 1)


# here is how to use odap_opag
# without user defined nLx
results <- odap_opag(
  data_in = Pop,
  nLx = NULL,
  method = "mono"
)

# resulting data
results$data_out$`India - M - 1971 - 356`$Pop_out
# figure (points - old data, line  - our Fit)
results$figures

# Now example with user defined nLx
# ---------------------------------------------------------------------------- #
# imagine we use India, 1971, males as in the example provided by DemoTools
data("mx1dt")
# here I calculate nLx
nLx <- mx1dt    %>%
  select(-mxB) %>% 
  as_tibble()  %>% 
  pivot_longer(c(mxM, mxF),
               names_to  = "sex",
               values_to = "mx") %>% 
  mutate(sex = str_sub(sex, start = 3)) %>% 
  filter(name == "India" | country_code == 356) %>% 
  filter(sex == "M", year == 1971) %>%
  group_by(name, country_code, sex, year) %>% 
  reframe(lt_single_mx(nMx = mx, Age = age)) %>% 
  select(country_code:sex, age = Age, nLx)


#  with user defined nLx
# With user defined nLx it will work slightly faster, because
# there is no need for package finding and uploading, uploading data and 
# calculating nLx from it, etc.
results2 <- odap_opag(
  data_in = Pop,
  nLx = nLx,
  method = "mono"
)

# Given the example we have here naturally results are exactly the same 
results2$data_out$`India - M - 1971 - 356`$Pop_out
results2$figures


# Now example with user defined nLx BUT as a column in data_in
# ---------------------------------------------------------------------------- #
pop_nLx <- Pop %>% 
  left_join(nLx)
  
results3 <- odap_opag(
  data_in = pop_nLx,
  nLx = NULL,
  method = "mono"
)

# and again the results are the same obviously
results3$data_out$`India - M - 1971 - 356`$Pop_out
results3$figures

# ---------------------------------------------------------------------------- #
# you can also try different methods

odap_opag(
  data_in = pop_nLx,
  nLx = NULL,
  method = "pclm"
)

odap_opag(
  data_in = pop_nLx,
  nLx = NULL,
  method = "uniform"  
  )

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# NEXT
# This example here is for the movepop function provided by Patrik
# The examples that it uses are his, I did not change them


set.seed(123)

# Create a synthetic population with realistic age structure
total_pop <- 1000000
age_dist <- c(rep(0.012, 5),    # 0-4: higher for young ages
              rep(0.011, 10),   # 5-14
              rep(0.010, 10),   # 15-24
              rep(0.009, 10),   # 25-34
              rep(0.008, 10),   # 35-44
              rep(0.007, 10),   # 45-54
              rep(0.006, 10),   # 55-64
              rep(0.005, 10),   # 65-74
              rep(0.003, 10),   # 75-84
              rep(0.002, 10),   # 85-94
              rep(0.001, 6))    # 95-100
         

age_dist <- age_dist / sum(age_dist)  # Normalize

# Split between males and females (slightly more males at birth, more females at older ages)
sex_ratio_by_age <- c(rep(1.05, 20), rep(1.02, 30), rep(0.98, 30), rep(0.85, 21))

female_prop <- 1 / (1 + sex_ratio_by_age)
male_prop <- 1 - female_prop

male_pop <- round(total_pop * age_dist * male_prop)
female_pop <- round(total_pop * age_dist * female_prop)

# Generate mortality rates (increasing with age)
male_mx <- c(0.005, 0.0005, rep(0.0002, 8),  # 0-9
             rep(0.0003, 10),                 # 10-19
             rep(0.0005, 10),                 # 20-29
             rep(0.0008, 10),                 # 30-39
             rep(0.0015, 10),                 # 40-49
             rep(0.003, 10),                  # 50-59
             rep(0.007, 10),                  # 60-69
             rep(0.015, 10),                  # 70-79
             rep(0.04, 10),                   # 80-89
             rep(0.1, 10),                    # 90-99
             0.25)                            # 100+

female_mx <- male_mx * 0.85  # Females have lower mortality


# Generate single-year ASFRs for ages 15-49
asfr <- c(rep(0.01, 5),    # 15-19: low fertility
          rep(0.08, 5),    # 20-24: increasing
          rep(0.10, 5),    # 25-29: peak
          rep(0.09, 5),    # 30-34: declining
          rep(0.05, 5),    # 35-39: lower
          rep(0.02, 5),    # 40-44: very low
          rep(0.005, 5))   # 45-49: minimal


# Run projection
# This returns a  list
result <- movepop(
  initial_date = 2020.5,
  desired_date = 2025.5,
  male_pop = male_pop,
  female_pop = female_pop,
  male_mx = male_mx,
  female_mx = female_mx,
  asfr = asfr,
  annual_net_migrants = 10000,
  age_format = "auto"
)

# same as
result <- movepop(
  initial_date = 2020.5,
  desired_date = 2025.5,
  male_pop = male_pop,
  female_pop = female_pop,
  male_mx = male_mx,
  female_mx = female_mx,
  asfr = asfr,
  annual_net_migrants = 10000,
  age_format = "single_year"
)

# Note: His convert_age_data just groups and ungroups ages (Demotools)
# Other functions are just for plotting and summing and printing


# Another example from his file with 5-year pop
male_pop <- c(48875, 164390, 173551, 130297, 101143, 73615, 60594, 55175, 
              49530, 46562, 39028, 27837, 22110, 18066, 15340, 13318, 
              12002, 6424)

female_pop <- c(47105, 159546, 168760, 119437, 92080, 70515, 58801, 53381, 
                46757, 41164, 33811, 24121, 19315, 16319, 14058, 12302, 
                11047, 5922)

male_mx <- c(0.12427, 0.01639, 0.00274, 0.00167, 0.00251, 0.00380, 0.00382, 
             0.00442, 0.00506, 0.00663, 0.00872, 0.01240, 0.01783, 0.02700, 
             0.04126, 0.06785, 0.11287, 0.21015)

female_mx <- c(0.11050, 0.01577, 0.00254, 0.00159, 0.00232, 0.00304, 0.00344, 
               0.00370, 0.00418, 0.00492, 0.00592, 0.00831, 0.01182, 0.01942, 
               0.03221, 0.05669, 0.09771, 0.19385)

asfr <- c(0.199, 0.478, 0.418, 0.321, 0.163, 0.071, 0.028)

# Run projection
result <- movepop(
  initial_date = 1973.58,
  desired_date = 1973.50,
  male_pop = male_pop,
  female_pop = female_pop,
  male_mx = male_mx,
  female_mx = female_mx,
  asfr = asfr,
  annual_net_migrants = -50000,
  age_format = "five_year"
)

# same as
result <- movepop(
  initial_date = 1973.58,
  desired_date = 1973.50,
  male_pop = male_pop,
  female_pop = female_pop,
  male_mx = male_mx,
  female_mx = female_mx,
  asfr = asfr,
  annual_net_migrants = -50000,
  age_format = "auto"
)
