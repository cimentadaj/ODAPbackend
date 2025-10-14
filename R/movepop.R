# movepop <- function(initial_date, 
#                     desired_date,
#                     male_pop,
#                     female_pop,
#                     male_mx,
#                     female_mx,
#                     asfr,
#                     annual_net_migrants = 0,
#                     age_groups = NULL,
#                     age_format = "auto") {
#   
#   # age format
#   age_format <- match.arg(age_format, c("single_year", "five_year", "auto"))
#   
#   # check if all incoming data have same number of ages
#   # Validate input lengths
#   if(length(unique(
#       c(length(male_pop), length(female_pop), length(male_mx), length(female_mx))
#       )) != 1) {
#     stop("All population and mortality vectors must have the same length")
#   }
#   
#   n_ages <- length(male_pop)
#   
#   # Auto-detect age format
#   # stop if age format is not single or five year
#   if (age_format == "auto") {
#     
#     age_format <- case_when(
#       n_ages == 18   ~ "five_year",
#       n_ages >= 101  ~ "single_year",
#       TRUE           ~ NA_character_
#     )
#     
#     if(is.na(age_format)) { 
#       
#       stop("Cannot auto-detect age format")
#       
#       }
#     
#   }
#   
#   
#   # groupAges()
#   
#   # Default age groups
#   if(is.null(age_groups)) {
#     
#     if(age_format == "five_year") { 
#       
#       ge_groups <- c(
#           "0-1",   "1-4",   "5-9",   "10-14", "15-19", "20-24", "25-29", 
#           "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
#           "65-69", "70-74", "75-79", "80+")
#     }
#     
#     if(age_format == "single_year") { 
#       
#       age_groups <- c(as.character(0:99), "100+")
#       
#     }
#     
#     
#     if(!age_format %in% c("single_year", "five_year")) { 
#       
#       age_groups <- c(as.character(0:(n_ages - 2)), paste0(n_ages - 1, "+"))
#       
#       }
#     
#   }
#   
#   
# 
#   # Reproductive indices (fertile ages)
#   repro_indices <- if (age_format == "five_year") 5:11 else {
#     switch(as.character(length(asfr)),
#            `35` = 16:50, `40` = 11:50, `45` = 11:55,
#            stop("ASFR must have 35, 40, or 45 values for single-year format"))
#   }
#   
#   # Births
#   births <- asfr * female_pop[repro_indices]
#   birth_age_groups <- if (age_format == "five_year") c("15-19","20-24","25-29","30-34","35-39","40-44","45-49") else
#     as.character(if (length(asfr)==35) 15:49 else if (length(asfr)==40) 10:49 else 10:54)
#   total_births <- sum(births)
#   
#   # Deaths
#   deaths <- male_mx * male_pop + female_mx * female_pop
#   total_deaths <- sum(deaths)
#   
#   # Vital rates
#   total_pop_initial <- sum(male_pop) + sum(female_pop)
#   crude_birth_rate <- total_births / total_pop_initial
#   crude_death_rate <- total_deaths / total_pop_initial
#   net_migration_rate <- annual_net_migrants / total_pop_initial
#   growth_rate <- crude_birth_rate - crude_death_rate + net_migration_rate
#   tfr <- if (age_format == "five_year") 5*sum(asfr) else sum(asfr)
#   
#   # Project population
#   time_diff <- desired_date - initial_date
#   total_pop_projected <- round(total_pop_initial * exp(growth_rate * time_diff))
#   adjustment_factor <- total_pop_projected / total_pop_initial
#   male_pop_projected <- round(adjustment_factor * male_pop)
#   female_pop_projected <- round(adjustment_factor * female_pop)
#   both_sexes_projected <- male_pop_projected + female_pop_projected
#   sex_ratio <- male_pop_projected / female_pop_projected
#   
#   # Age summary
#   age_summary <- tibble(
#     age_group = age_groups,
#     both_sexes = both_sexes_projected,
#     male = male_pop_projected,
#     female = female_pop_projected,
#     sex_ratio = round(sex_ratio,6)
#   )
#   
#   # Special summaries
#   all_ages <- tibble(
#     age_group = "All ages",
#     both_sexes = sum(both_sexes_projected),
#     male = sum(male_pop_projected),
#     female = sum(female_pop_projected),
#     sex_ratio = sum(male_pop_projected)/sum(female_pop_projected)
#   )
#   
#   special_summaries <- list(all_ages = all_ages)
#   
#   if (age_format == "five_year") {
#     special_summaries$under_5 <- tibble(
#       age_group = "0-4",
#       both_sexes = sum(both_sexes_projected[1:2]),
#       male = sum(male_pop_projected[1:2]),
#       female = sum(female_pop_projected[1:2]),
#       sex_ratio = sum(male_pop_projected[1:2])/sum(female_pop_projected[1:2])
#     )
#   } else {
#     df <- tibble(
#       age = 0:(length(both_sexes_projected)-1),
#       both_sexes = both_sexes_projected,
#       male = male_pop_projected,
#       female = female_pop_projected
#     ) %>%
#       mutate(age_group = case_when(
#         age <= 4 ~ "0-4",
#         age >= 100 ~ "100+",
#         TRUE ~ paste0(floor(age/5)*5,"-",floor(age/5)*5+4)
#       )) %>%
#       group_by(age_group) %>%
#       summarise(
#         both_sexes = sum(both_sexes),
#         male = sum(male),
#         female = sum(female),
#         .groups="drop"
#       ) %>%
#       mutate(sex_ratio = male/female)
#     special_summaries$age_groups_5yr <- df
#   }
#   
#   # Assemble results
#   results <- list(
#     initial_date = initial_date,
#     desired_date = desired_date,
#     time_interval = time_diff,
#     age_format = age_format,
#     crude_birth_rate = crude_birth_rate,
#     crude_death_rate = crude_death_rate,
#     net_migration_rate = net_migration_rate,
#     growth_rate = growth_rate,
#     total_fertility_rate = tfr,
#     initial_population = total_pop_initial,
#     projected_population = total_pop_projected,
#     adjustment_factor = adjustment_factor,
#     births_by_age = tibble(age_group=birth_age_groups, births=births, asfr=asfr),
#     total_births = total_births,
#     deaths_by_age = tibble(age_group=age_groups, deaths=deaths, male_mx=male_mx, female_mx=female_mx),
#     total_deaths = total_deaths,
#     population_projection = if(age_format=="five_year") bind_rows(all_ages,special_summaries$under_5,age_summary) else age_summary,
#     special_summaries = special_summaries,
#     annual_net_migrants = annual_net_migrants
#   )
#   
#   return(results)
# }



#' Project Population Between Two Dates Using Age-Specific Rates
#'
#' The `movepop()` function projects a population from an initial date to a desired date
#' using age-specific fertility (ASFR) and mortality rates. It supports single-year or 
#' five-year age group formats and optionally accounts for net migration. 
#' The function produces both projected population by age and sex and summary statistics.
#'
#' @param initial_date Numeric or Date. The starting year/date of the population.
#' @param desired_date Numeric or Date. The year/date to which the population is projected.
#' @param male_pop Numeric vector of male population counts by age.
#' @param female_pop Numeric vector of female population counts by age.
#' @param male_mx Numeric vector of male mortality rates by age.
#' @param female_mx Numeric vector of female mortality rates by age.
#' @param asfr Numeric vector of age-specific fertility rates corresponding to female ages.
#' @param annual_net_migrants Numeric scalar for annual net migration to include in growth (default 0).
#' @param age_groups Optional character vector of age group labels. If `NULL`, default labels are assigned.
#' @param age_format Character specifying age structure: `"single_year"`, `"five_year"`, or `"auto"` (default `"auto"`). When `"auto"`, the function infers format from the length of population vectors.
#'
#' @details
#' - The function ensures that population and mortality vectors have equal lengths.
#' - If `age_format` is `"auto"`, lengths of 18 imply `"five_year"` and lengths ≥101 imply `"single_year"`.
#' - Age groups are automatically generated if not provided, and ASFR values are aligned starting at the age group `"15"` (or equivalent).
#' - Projected populations are computed using exponential growth based on crude birth rate, crude death rate, and net migration rate.
#' - The function returns both age-specific projected populations and summary statistics.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{\code{initial_summaries}}{A tibble summarising total births, deaths, crude rates, growth rate, total population, and adjustment factor.}
#'   \item{\code{projected_summaries}}{A tibble summarising projected total population by sex and overall sex ratio.}
#'   \item{\code{data}}{A tibble with age-specific populations, ASFR, births, deaths, projected populations by sex, both-sexes totals, and sex ratios.}
#' }
#'
#' @examples
#' \dontrun{
#'male_pop <- c(48875, 164390, 173551, 130297, 101143, 73615, 60594, 55175, 
#'              49530, 46562, 39028, 27837, 22110, 18066, 15340, 13318, 
#'#'              12002, 6424)
#'
#'female_pop <- c(47105, 159546, 168760, 119437, 92080, 70515, 58801, 53381, 
#'                46757, 41164, 33811, 24121, 19315, 16319, 14058, 12302, 
#'                11047, 5922)
#'
#'male_mx <- c(0.12427, 0.01639, 0.00274, 0.00167, 0.00251, 0.00380, 0.00382, 
#'             0.00442, 0.00506, 0.00663, 0.00872, 0.01240, 0.01783, 0.02700, 
#'             0.04126, 0.06785, 0.11287, 0.21015)
#'
#'female_mx <- c(0.11050, 0.01577, 0.00254, 0.00159, 0.00232, 0.00304, 0.00344, 
#'               0.00370, 0.00418, 0.00492, 0.00592, 0.00831, 0.01182, 0.01942, 
#'              0.03221, 0.05669, 0.09771, 0.19385)
#'
#'asfr <- c(0.199, 0.478, 0.418, 0.321, 0.163, 0.071, 0.028)
#'
#' res <- movepop(
#'initial_date = 1973.58,
#'desired_date = 1973.50,
#'male_pop = male_pop,
#'female_pop = female_pop,
#'male_mx = male_mx,
#'female_mx = female_mx,
#'asfr = asfr,
#'annual_net_migrants = -50000,
#'age_format = "five_year"
#'
#' res$initial_summaries
#' res$projected_summaries
#' head(res$data)
#' }
#'
#' @importFrom dplyr mutate summarise case_when
#' @importFrom stringr str_detect
#' @importFrom magrittr %>% 
#' @importFrom tibble tibble
#' @export



# make age mandatory arg. remove age_format, rename arguments
movepop <- function(initial_date, 
                    desired_date,
                    male_pop,
                    female_pop,
                    male_mx,
                    female_mx,
                    asfr,
                    annual_net_migrants = 0,
                    age_groups = NULL,
                    age_format = "auto") {
  
  # age format
  age_format <- match.arg(age_format, c("single_year", "five_year", "auto"))
  
  lengths_vec <- c(
    length(male_pop),
    length(female_pop),
    length(male_mx),
    length(female_mx)
  )
  
  if(length(unique(lengths_vec)) != 1) {
    stop("All population and mortality vectors must have the same length (ages)")
  }
  
  # collect 
  data <- tibble(
    male_pop   = male_pop,
    female_pop = female_pop,
    male_mx    = male_mx,
    female_mx  = female_mx
  )
  
  n_ages <- lengths_vec[1]
  
  # Auto-detect age format
  # stop if age format is not single or five year
  if(age_format == "auto") {
    
    age_format <- case_when(
      n_ages == 18                           ~ "five_year",
      n_ages %in% c(81, 86) | n_ages >= 101  ~ "single_year",
      TRUE                                   ~ NA_character_
    )
    
  }
  
  if(is.na(age_format)) { 
    
    stop("Cannot auto-detect age format")
    
  }
  
  # Default age groups
  if(is.null(age_groups)) {
    
    # for 5-year age groups
    if(age_format == "five_year") { 
      
      age_groups <- c(
        "0-1",   "1-4",   "5-9",   "10-14", "15-19", "20-24", "25-29", 
        "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
        "65-69", "70-74", "75-79", "80+")
    } else { 
    
    # for single age groups
      
      age_groups <- c(as.character(0:(n_ages - 2)), paste0(n_ages - 1, "+"))
      
      if(!length(asfr) %in% c(35, 40, 45)) { 
        
        stop("ASFR must have 35, 40, or 45 values for single-year format")
        
      }
    }
    }
  
  # asfr join
  vec <- vector("numeric", length = nrow(data))
  ind <- which(str_detect(age_groups, "15"))
  vec[ind:(ind + length(asfr) - 1)] <- asfr
  
  data <- data %>% 
    mutate(age    = age_groups,
           asfr   = vec,
           births = asfr * female_pop,
           deaths =  male_mx * male_pop + female_mx * female_pop)
  
  summaries <- data %>% 
    summarise(total_births        = sum(births),
              total_deaths        = sum(deaths),
              total_pop_initial   = sum(male_pop) + sum(female_pop),
              crude_birth_rate    = total_births / total_pop_initial,
              crude_death_rate    = total_deaths / total_pop_initial,
              net_migration_rate  = annual_net_migrants / total_pop_initial,
              growth_rate         = crude_birth_rate - crude_death_rate + net_migration_rate,
              age_format          = age_format,
              tfr                 = ifelse(age_format == "five_year", 5 * sum(asfr), sum(asfr)),
              desired_date        = desired_date,
              initial_date        = initial_date,
              time_diff           = desired_date - initial_date,
              total_pop_projected = total_pop_initial * exp(growth_rate * time_diff),
              adjustment_factor   = total_pop_projected / total_pop_initial)
  
  
  projected_data <- data %>% 
    mutate(male_pop_projected   = summaries$adjustment_factor * male_pop,
           female_pop_projected = summaries$adjustment_factor * female_pop,
           both_sexes_projected = male_pop_projected + female_pop_projected,
           sex_ratio            = male_pop_projected / female_pop_projected
    )
  
  summaries_proj <- projected_data %>%
    summarise(age_group  = "All ages",
              both_sexes = sum(both_sexes_projected),
              male       = sum(male_pop_projected),
              female     = sum(female_pop_projected),
              sex_ratio  = sum(male_pop_projected) / sum(female_pop_projected))
  
  
  # Assemble results
  results <- list(
    initial_summaries   = summaries,
    projected_summaries = summaries_proj,
    data                = projected_data
  )
  
  return(results)
}
