

# if pop is given then use pop
# if not given find from popAge1dt


# same with SRB, ASFR nLx



"World"

Males_single <- popAge1dt |>
  filter(country_code == 900,
         year == 1999) |> 
  select(year, age, pop = popM) |> 
  mutate(year = year + 1,
         cohort = year - age - 1) %>% 
  pull(pop)

refDate <- 2000

Age <- Males_single$age

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
         ax = if_else(age == 0, lt_rule_1a0_ak(M0 = mx, Sex = "f"), .5),
         qx = lt_id_ma_q(nMx = mx, nax = ax, AgeInt = age_int)) |> 
  arrange(cohort, age) |> 
  group_by(cohort) |> 
  mutate(lx = lt_id_q_l(nqx = qx, radix = 1),
         dx = lx * qx,
         Lx = lx - dx * (1 - ax),
         SxRev = Lx / lead(Lx, default = 1)) |> 
  ungroup() |> 
  arrange(year, age) |> 
  group_by(year) |> 
  mutate(
    lxp = lt_id_q_l(nqx = qx, radix = 1),
    dxp = lxp * qx,
    Lxp = lxp - dxp * (1-ax),
    SxRev = if_else(year == max(year), Lxp / lead(Lxp), SxRev)) |> 
  ungroup() |> 
  filter(age < 100) |> 
  select(cohort, year, age, SxRev) |> 
  arrange(cohort, -age) |> 
  group_by(cohort) |> 
  # inflation for reverse-surviving
  mutate(inflation_factor = cumprod(SxRev))



# minimum set
location = NULL
Age = NULL
Females_single = NULL
Males_single = NULL
nLxFemale = NULL
nLxMale = NULL
nLxDatesIn = NULL
AsfrMat = NULL
AsfrDatesIn = NULL
SRB = NULL
SRBDatesIn = NULL
radix = 1
verbose = TRUE
refDate      <- 2000
country_code <- 900





basepop_single <- function(location = NULL,
                           refDate,
                           Age = NULL,
                           country_code = NULL,
                           Females_single = NULL,
                           Males_single = NULL,
                           nLxFemale = NULL,
                           nLxMale = NULL,
                           nLxDatesIn = NULL,
                           AsfrMat = NULL,
                           AsfrDatesIn = NULL,
                           ...,
                           SRB = NULL,
                           SRBDatesIn = NULL,
                           radix = 1,
                           verbose = TRUE) {
  
  
  # options from DemoTools version
  options(basepop_verbose = verbose)
  on.exit(options(basepop_verbose = NULL))
  
  
  # Jan 1 2000 female pop; 
  # note:  1999 means dec 31, 1999, so we treat as jan 1, 2000.
  # we need these dates to filter period
  refDate       <- dec.date(refDate) - 1
  refDate_start <- refDate - 9

  # we need minimum of date and country to run a function
  if(is.null(refDate) | is.null(country_code)) { 
    
    stop("At least refDate and country_code should be provided for function to work")
    
  }
  
  
  # here we attach the latest wpp package available
  installed_wpp <- grep("^wpp\\d{4}$", rownames(installed.packages()), value = TRUE)
  if(length(installed_wpp) == 0) stop("No wpp package installed.")
  latest_wpp <- sort(installed_wpp, decreasing = TRUE)[1]
  suppressPackageStartupMessages(library(latest_wpp, character.only = TRUE))
  
   
  # if user did not provide population we can calculate it from wpp
  if(is.null(Females_single) | is.null(Males_single)) { 
 
  data("popAge1dt", package = latest_wpp)
  
  }
  
  # this data is for future calculations
  data("mx1dt", package = latest_wpp)
  
  # if user provided Population vector we turn it into tibble
  if(!is.null(Females_single)) {
    
    Females_single <- tibble(pop = Females_single,
                             year   = refDate + 1,
                             age = row_number() - 1,
                             cohort = year - age - 1)
    
    
  }
  
  # same for Males
  if(!is.null(Males_single)) {
    
    Males_single <- tibble(pop    = Males_single,
                           year   = refDate + 1,
                           age = row_number() - 1,
                           cohort = year - age - 1)
    
    
  }
  
  # if user did not provide Population
  if(is.null(Females_single)) {
    
    Females_single <- popAge1dt |>
      filter(country_code == !!country_code,
             year == refDate) |> 
      select(year, age, pop = popF) |> 
      mutate(year = year + 1,
             cohort = year - age - 1)    
    
  }
  
  # same operation for males
  if(is.null(Males_single)) {
    
    Males_single <- popAge1dt |>
      filter(country_code == !!country_code,
             year == refDate) |> 
      select(year, age, pop = popM) |> 
      mutate(year = year + 1,
             cohort = year - age - 1)
    
  }
  
  # Age setup
  if (!is.null(Age)) {
    stopifnot(is_single(Age))
    stopifnot(length(Age) == nrow(Females_single))
    
  } else {
    
      Age <- as.integer(sort(unique(Females_single$age)))
      
  }
  
  nLxFemale = NULL
  nLxMale = NULL
  nLxDatesIn = NULL
  AsfrMat = NULL
  AsfrDatesIn = NULL
  SRB = NULL
  SRBDatesIn
  # if user did not 
  
  # this should be changed
  if(!is.null(nLxFemale)) { 
    
    mxF <- mx1dt |>
      filter(country_code == !!country_code,
             between(year, refDate_start, refDate)) |>
      as_tibble() |>
      select(year, age, mx = mxF) |>
      # could try to warp to PC shape here,
      # but uncertain infants. Maybe using
      # an a0 assumption it'd be doable.
      # need cohorts to structure reverse survival
      mutate(
        cohort = year - age - 1,
        age_int = 1,
        ax = if_else(age == 0, lt_rule_1a0_ak(M0 = mx, Sex = "f"), .5),
        qx = lt_id_ma_q(nMx = mx, nax = ax, AgeInt = age_int)
      ) |>
      arrange(cohort, age) |>
      group_by(cohort) |>
      mutate(
        lx = lt_id_q_l(nqx = qx, radix = 1),
        dx = lx * qx,
        Lx = lx - dx * (1 - ax),
        SxRev = Lx / lead(Lx, default = 1)
      ) |>
      ungroup() |>
      arrange(year, age) |>
      group_by(year) |>
      mutate(
        lxp = lt_id_q_l(nqx = qx, radix = 1),
        dxp = lxp * qx,
        Lxp = lxp - dxp * (1 - ax),
        SxRev = if_else(year == max(year), Lxp / lead(Lxp), SxRev)
      ) |>
      ungroup() |>
      filter(age < 100) |>
      select(cohort, year, age, SxRev) |>
      arrange(cohort, -age) |>
      group_by(cohort) |>
      # inflation for reverse-surviving
      mutate(inflation_factor = cumprod(SxRev))
    
  }
  
  
  if(is.null(nLxFemale)) {
  
  mxF <- mx1dt |>
    filter(country_code == !!country_code,
           between(year, refDate_start, refDate)) |>
    as_tibble() |>
    select(year, age, mx = mxF) |>
    # could try to warp to PC shape here,
    # but uncertain infants. Maybe using
    # an a0 assumption it'd be doable.
    # need cohorts to structure reverse survival
    mutate(
      cohort = year - age - 1,
      age_int = 1,
      ax = if_else(age == 0, lt_rule_1a0_ak(M0 = mx, Sex = "f"), .5),
      qx = lt_id_ma_q(nMx = mx, nax = ax, AgeInt = age_int)
    ) |>
    arrange(cohort, age) |>
    group_by(cohort) |>
    mutate(
      lx = lt_id_q_l(nqx = qx, radix = 1),
      dx = lx * qx,
      Lx = lx - dx * (1 - ax),
      SxRev = Lx / lead(Lx, default = 1)
    ) |>
    ungroup() |>
    arrange(year, age) |>
    group_by(year) |>
    mutate(
      lxp = lt_id_q_l(nqx = qx, radix = 1),
      dxp = lxp * qx,
      Lxp = lxp - dxp * (1 - ax),
      SxRev = if_else(year == max(year), Lxp / lead(Lxp), SxRev)
    ) |>
    ungroup() |>
    filter(age < 100) |>
    select(cohort, year, age, SxRev) |>
    arrange(cohort, -age) |>
    group_by(cohort) |>
    # inflation for reverse-surviving
    mutate(inflation_factor = cumprod(SxRev))
  }
  
  expF <- Females_single |> 
    select(cohort, pop) |> 
    right_join(mxF, by = join_by(cohort)) |> 
    mutate(pop_hat = pop * inflation_factor) |> 
    select(year, age, pop = pop_hat) |> 
    bind_rows(Females_single |> select(year, age, pop)) |> 
    filter(between(age, 15, 50)) |> 
    arrange(age, year) |> 
    mutate(pop_1p1  = lead(pop),
           exposure = (pop + pop_1p1) / 2) |>
    filter(age < 50)
  
  if(!is.null(AsfrMat)) {
    
    AsfrMat <- AsfrMat %>% 
      pivot_longer(-c(country_code, name, age),
                   names_to = "year",
                   values_to = "asfr") %>% 
      mutate(year = as.numeric(year)) %>% 
      select(-name)
    
    
  }
    
  
  if(is.null(AsfrMat)) { 
    
    AsfrMat <- download_Asfr(Asfrmat = NULL, 
                             location = country_code, 
                             AsfrDatesIn = refDate_start:refDate,
                             method      = "linear",
                             output      = "single") %>% 
      pivot_longer(-c(country_code, name, age),
                   names_to = "year",
                   values_to = "asfr") %>% 
      mutate(year = as.numeric(year)) %>% 
      select(-name)
    
  }
  
  
  Bt <- left_join(expF, AsfrMat , by = join_by(year, age)) |>
    filter(year < refDate + 1) |>
    mutate(Bx = asfr * exposure) |>
    group_by(year) |>
    summarize(B = sum(Bx)) |>
    mutate(age = refDate - year) |>
    select(-year)
  
  
  if(!is.null(SRB)) {
    
    SRB <- tibble(SRB = SRB,
                  cohort = refDate_start:refDate)
    
  }
  
  if(is.null(SRB)) { 
  SRB <- download_SRB(SRB = NULL,
                      location = country_code,
                      DatesOut = refDate_start:refDate,
                      verbose = TRUE)
  
  SRB <- tibble(SRB = SRB,
                cohort = refDate_start:refDate)
  
  }
  
  
  # final part
  pop_hat <- mx1dt |>
    filter(country_code == !!country_code) |>
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
                        0.5),
           qx = lt_id_ma_q(nMx = mx, nax = ax, AgeInt = age_int)) |>
    filter(between(cohort, refDate_start,refDate),
           between(year, refDate_start,(refDate+1)),
           age < 10) |>
    arrange(cohort, age) |>
    group_by(cohort) |>
    mutate(lx = lt_id_q_l(nqx = qx, radix = 1),
           dx = lx * qx,
           Lx = lx - dx * (1-ax)) |>
    filter(year == max(year)) |>
    select(age, Lx, cohort) |>
    left_join(Bt, by = join_by(age)) |>
    left_join(SRB, by = join_by(cohort)) |>
    mutate(pop_hat = Lx * B,
           popF = pop_hat * 1 / (1 + SRB),
           popM = pop_hat * SRB / (1 + SRB)) %>% 
    ungroup()
  
  
  pop_hat |>
    select(age, pop = popF) |>
    ggplot(aes(x = age, y = pop)) +
    geom_line() +
    geom_line(data = Females_single |> filter(age < 10),color = "red")
  
  
  pop_hat |>
    select(age, pop = popM) |>
    ggplot(aes(x = age, y = pop)) +
    geom_line() +
    geom_line(data = Males_single |> filter(age < 10),color = "red")
  
  # this can be read in from the data
  # we just can take birth from wpp???? we can generate female exposure based on female population
  # reverse survive wpp and take new exposures
  # utils download function 294 interp_co_download_mortailty demotools 
  # gets mx then calculates Sx from it. 
  # first and last matrix leslie output will be discounted in case it is not cleran dates
  # take the product of the cohort diagonals year age Sx and then make a cohort variable year - age - 1
  # and then arrange cohort age group by cohort summarize Sx = prod(Sx) - survive births forward to the census
  # use Lx downloader and utild for baasepopsingle
  
}