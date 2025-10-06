library(tidyverse)
library(wpp2024)
# ------
data(popAge1dt)
data(mx1dt)
# Jan 1 2000 female pop; 
# note:  1999 means dec 31, 1999, so we treat as jan 1, 2000.
popF <-
  popAge1dt |> 
  filter(country_code == 900, 
         year == 1999) |> 
  select(year, age, pop = popF) |> 
  mutate(year = year + 1,
         cohort = year - age - 1)

mxF <- mx1dt |> 
  filter(country_code == 900, 
         between(year, 1990, 1999)) |> 
  as_tibble() |> 
  select(year, age, mx = mxF) |>
  # could try to warp to PC shape here, 
  # but uncertain infants. Maybe using
  # an a0 assumption it'd be doable.
  
  # need cohorts to structure reverse survival
  mutate(cohort = year - age - 1,
         age_int = 1,
         ax = if_else(age == 0, 
                      lt_rule_1a0_ak(M0 = mx, Sex = "f"),
                      .5),
         qx = lt_id_ma_q(nMx = mx, nax = ax, AgeInt = age_int)) |> 
  arrange(cohort, age) |> 
  group_by(cohort) |> 
  mutate(lx = lt_id_q_l(nqx = qx, radix = 1),
         dx = lx * qx,
         Lx = lx - dx * (1-ax),
         SxRev = Lx / lead(Lx, default = 1)) |> 
  ungroup() |> 
  arrange(year, age) |> 
  group_by(year) |> 
  mutate(
    lxp = lt_id_q_l(nqx = qx, radix = 1),
    dxp = lxp * qx,
    Lxp = lxp - dxp * (1-ax),
    SxRev = if_else(year == max(year),
                         Lxp / lead(Lxp),
                         SxRev)) |> 
  ungroup() |> 
  filter(age < 100) |> 
  select(cohort, year, age, SxRev) |> 
  arrange(cohort,-age) |> 
  group_by(cohort) |> 
  # inflation for reverse-surviving
  mutate(inflation_factor = cumprod(SxRev))

# get exposure via reverse survival
expF <-
  popF |> 
  select(cohort, pop) |> 
  right_join(mxF,by=join_by(cohort)) |> 
  mutate(pop_hat = pop * inflation_factor) |> 
  select(year, age, pop = pop_hat) |> 
  bind_rows(popF |> select(year,age,pop)) |> 
  filter(between(age, 15,50)) |> 
  arrange(age, year) |> 
  mutate(pop_1p1 = lead(pop),
         exposure = (pop + pop_1p1) / 2) |> 
  filter(age<50)
  
# each location year sums to 100%
data(percentASFR1dt)
# use TFR as period scalar
data(tfr1dt)

# get asfr:
asfr <-
  left_join(percentASFR1dt,tfr1dt,by=join_by(year, country_code,name)) |> 
  filter(country_code == 900, 
         between(year, 1990,1999)) |> 
  mutate(asfr = pasfr / 100 * tfr)

Bt <- 
  left_join(expF, asfr |> select(-name), by = join_by(year, age)) |> 
  filter(year < 2000) |> 
  mutate(Bx = asfr * exposure) |> 
  group_by(year) |> 
  summarize(B = sum(Bx)) |> 
  mutate(age = 2000-year-1) |> 
  select(-year)

# this now needs to survive forward until year 2000

SRB <- 1.05

# this chunk might have a misalignment, and still needs
# some care
pop_hat<-
  mx1dt |> 
  filter(country_code == 900) |> 
  as_tibble() |> 
  select(year, age, mx = mxF) |>
  # could try to warp to PC shape here, 
  # but uncertain infants. Maybe using
  # an a0 assumption it'd be doable.
  
  # need cohorts to structure reverse survival
  mutate(cohort = year - age - 1,
         age_int = 1,
         ax = if_else(age == 0, 
                      lt_rule_1a0_ak(M0 = mx, Sex = "f"),
                      .5),
         qx = lt_id_ma_q(nMx = mx, nax = ax, AgeInt = age_int)) |> 
  filter(between(cohort,1990,1999),
         between(year,1990,2000),
         age < 10) |> 
  arrange(cohort, age) |> 
  group_by(cohort) |> 
  mutate(lx = lt_id_q_l(nqx = qx, radix = 1),
         dx = lx * qx,
         Lx = lx - dx * (1-ax)) |> 
  filter(year == max(year)) |> 
  select(age, Lx) |> 
  left_join(Bt, by = join_by(age)) |> 
  mutate(pop_hat = Lx * B,
         popF = pop_hat * 1 / (1+SRB),
         popM = pop_hat * SRB / (1+SRB))

# reasonable comparison!
pop_hat |> 
  select(age, pop = popF) |> 
  ggplot(aes(x = age, y = pop)) +
  geom_line() +
  geom_line(data = popF |> filter(age < 10),color = "red")
