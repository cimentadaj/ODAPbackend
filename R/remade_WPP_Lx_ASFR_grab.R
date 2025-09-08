
# !!!!!!!!!!!!!!!!!!!!!!!!!!!! NOTE:
# This script remakes the DemoTools functions
# DemoTools::downloadSRB()
# DemoTools::downloadnLx()
# DemoTools::downloadAsfr()
# By allowing to detect the wpp version and 1 or 5 year age groups
# I have closely followed the logic of original functions
# performance can be improved if approved in future 


# nLx        = NULL
# location   = "Argentina"
# gender     = "both"
# nLxDatesIn = 1950:2030
# method     = "linear"
# 
# 
# refDate  <- 1986
# location <- "Brazil"

download_Lx <- function(nLx      = NULL,
                        location = NULL,
                        gender   = NULL,
                        nLxDatesIn = NULL,
                        method = "linear",
                        ...) {
  
  verbose <- getOption("basepop_verbose", TRUE)
  
  # if nLx is provided by user just return back the nLx datatable
  if(!is.null(nLx)) {
    
    return(nLx)
    
  }
  
  # until the end of function
  if(is.null(nLx)) {
    
    # if no location return error
    if(is.null(location)) {
      stop("You need to provide a location to download the data for nLx")
    }
    
    # since we will need to download data anyway, I have changed the
    # function structure a little bit.
    # We first download data, then we ding if ID is provided,
    # then we do filtering and all other checks.
    # I do not think this will change the performance of the function
    # ------------------------------------------------------ #
    # NEW
    # I check which wpp versions are available on the machine
    # if none we stop
    # then I use the latest available for the data grab
    
    # installed wpp versions
    installed_wpp <- grep("^wpp\\d{4}$", rownames(installed.packages()), value = TRUE)
    
    # stop if none
    if(length(installed_wpp) == 0) {
      
      stop("No wpp package installed.")
      
    }
    # find the lates one
    latest_wpp <- sort(installed_wpp, decreasing = TRUE)[1]
    
    # download mx1dt data from the latest package version
    data("mx1dt", package = latest_wpp)
    
    # if any date chosen is less then 1950 or more than wpp version + 1
    if(any(nLxDatesIn < 1950, nLxDatesIn > (parse_number(latest_wpp) + 1))) {
      cat(paste0(
        "Careful, choosing beyond range 1950-",
        parse_number(latest_wpp)
      ))
      
    }
    
    # find location code from the provided location
    # if location is misspelled return NA
    location_code <- mx1dt %>%
      subset(name %in% location, select = country_code) %>%
      unique() %>%
      as.numeric()
    
    # ------------------------------------------------------ #
    # if we need message print it
    if(verbose) {
      cat(
        paste0(
          "Downloading nLx data for ",
          location,
          ", years ",
          paste(nLxDatesIn, collapse = ", "),
          ", gender ",
          gender
        ),
        sep = "\n"
      )
    }
    
    # standardize input strings to match what we expect
    sex_code <- ifelse(tolower(gender) == "both",
                       "b",
                       ifelse(
                         tolower(gender) == "female",
                         "f",
                         ifelse(tolower(gender) == "male", "m", NA)
                       ))
    
    # sex standardization
    Sex_mortlaws <- ifelse(sex_code == "b", "total", tolower(gender))
    
    stopifnot(`Invalid sex name, please set it to 'both', 'male' or 'female'` = !is.na(sex_code))
    
    # here some data wrangling going on, including
    # calulation of interp()
    # then lt_sinle Lx calculation
    
    out <- mx1dt %>%
      as_tibble() %>%
      filter(country_code %in% location_code,
             year < parse_number(latest_wpp) + 1) %>%
      pivot_longer(-c(country_code, name, year, age),
                   names_to  = "sex",
                   values_to = "mx") %>%
      mutate(sex = str_remove(sex, "mx"), sex = tolower(sex)) %>%
      subset(sex %in% sex_code) %>%
      pivot_wider(names_from  = year, values_from = mx) %>%
      select(-age) %>%
      group_nest(country_code, name, sex) %>%
      # interpolate
      mutate(data = map(
        data,
        ~ interp(
          .x,
          as.numeric(names(.x)),
          as.numeric(nLxDatesIn),
          extrap = TRUE,
          method = method,
          ...
        ) %>%
          as_tibble()
      )) %>%
      unnest(data) %>% 
      pivot_longer(-c(country_code, name, sex),
                   names_to  = "year",
                   values_to = "mx") %>% 
      group_nest(country_code, name, sex, year) %>%
      # calculate lifetable
      mutate(data = map(data, ~ lt_single_mx(nMx = .x$mx) %>%
                          select(age = Age, nLx))) %>%
      unnest(data) %>%
      # wide format
      pivot_wider(names_from  = year, 
                  values_from = nLx)
    return(out)
    
  }
}



# Asfrmat     = NULL
# location    = "Argentina"
# AsfrDatesIn = 1950:2030


download_Asfr <- function(Asfrmat     = NULL,
                          location    = NULL,
                          AsfrDatesIn = NULL,
                          method      = "linear",
                          ...) {
  
  
  verbose <- getOption("basepop_verbose", TRUE)
  
  
  if (!is.null(Asfrmat)) {
    return(Asfrmat)
  }
  
  if (is.null(location)) {
    stop("You need to provide a location to download the data for Asfrmat")
  }
  
  # ------------------------------------------------------ #
  # installed wpp versions
  installed_wpp <- grep("^wpp\\d{4}$", rownames(installed.packages()), value = TRUE)
  
  # stop if none
  if(length(installed_wpp) == 0) { 
    stop("No wpp package installed.")
  }
  # find the lates one
  latest_wpp <- sort(installed_wpp, decreasing = TRUE)[1]
  
  # download mx1dt data from the latest package version
  data("percentASFR1dt", package = latest_wpp)
  data("tfr1",           package = latest_wpp)
  
  # if any date chosen is less then 1950 or more than wpp version + 1
  if (any(AsfrDatesIn < 1950, AsfrDatesIn > (parse_number(latest_wpp) + 1))) {
    cat(paste0(
      "Careful, choosing beyond range 1950-",
      parse_number(latest_wpp)
    ))
    
  }
  # ------------------------------------------------------ #
  # find location code from the provided location
  # if location is misspelled return NA
  location_code <- percentASFR1dt %>%
    as_tibble() %>% 
    subset(name %in% location, select = country_code) %>%
    unique() %>%
    as.numeric()
  
  if(verbose) {
    cat(paste0(
      "Downloading ASFR data for ",
      location,
      ", years ",
      paste(AsfrDatesIn, collapse = ", ")
    ),
    sep = "\n")
  }
  
  age <- unique(percentASFR1dt$age)
  
  tfr <- tfr1 %>% 
    as_tibble() %>% 
    pivot_longer(-c(country_code, name),
                 names_to  = "year",
                 values_to = "tfr") %>% 
    subset(country_code %in% location_code &
             year < parse_number(latest_wpp) + 1) %>% 
    mutate(year = as.integer(year))
  
  out <- percentASFR1dt %>% 
    as_tibble() %>% 
    subset(country_code %in% location_code &
             year < parse_number(latest_wpp) + 1) %>%
    left_join(tfr) %>% 
    # create asfr
    mutate(asfr = (pasfr / 100) * tfr) %>%
    select(-c(pasfr, tfr)) %>% 
    # wide format
    pivot_wider(names_from  = year, 
                values_from = asfr) %>%
    select(-age) %>% 
    group_nest(country_code, name) %>%
    # interpolate
    mutate(data = map(
      data,
      ~ interp(
        .x,
        as.numeric(names(.x)),
        as.numeric(AsfrDatesIn),
        extrap = TRUE,
        method = method,
        ...
      ) %>%
        as_tibble()
    )) %>%
    unnest(data) %>%
    mutate(age = age)
  
  return(out)
}

# !!!!!!!!!!!!!!!!!!! DISREGARD THIS CODE ITS AN OLD ATTEMPT !!!!!!!!!!!!!!!!!!!!!


# download_SRB <- function(SRB, location, DatesOut, verbose = TRUE) {
#   
#   
#   if (!is.null(SRB)) {
#     if (length(SRB) > 3)
#       stop("SRB can only accept three dates at maximum")
#     rep_times <- 3 - length(SRB)
#     SRB <- c(SRB, rep(SRB, times = rep_times))
#     return(stats::setNames(SRB[1:3], DatesOut))
#   }
#   
#   
#   if (length(DatesOut) > 3) { 
#     stop("SRB can only accept three dates at maximum")
#   }
#   
#   
#   installed_wpp <- grep("^wpp\\d{4}$", rownames(installed.packages()), value = TRUE)
#   
#   # stop if none
#   if(length(installed_wpp) == 0) { 
#     stop("No wpp package installed.")
#   }
#   # find the lates one
#   latest_wpp <- sort(installed_wpp, decreasing = TRUE)[1]
#   
#   # download mx1dt data from the latest package version
#   data("sexRatio1", package = latest_wpp)
#   
#   # WPP2019_births <- DemoToolsData::WPP2019_births
#   
#   
#   SRB_default <- round((1 - 0.4886) / 0.4886, 3)
#   
#   
#   location_code <- sexRatio1 %>%
#     as_tibble() %>% 
#     subset(name %in% location, select = country_code) %>%
#     unique() %>%
#     as.numeric()
#   
#   
#   
#   if (is.na(location_code)) {
#     if (verbose) {
#       cat(paste(
#         location,
#         "not available in wpp"
#       ))
#       cat(paste("Assuming SRB to be", SRB_default, "\n"))
#     }
#     return(stats::setNames(rep(SRB_default, 3), DatesOut))
#   }
#   
#   if(verbose) {
#     cat(
#       paste0(
#         "\nbirths not provided. Downloading births for ",
#         location,
#         ", for years between ",
#         round(DatesOut[1], 1),
#         " and ",
#         round(DatesOut[length(DatesOut)], 1),
#         "\n"
#       )
#     )
#   }
#   
#   dt <- sexRatio1 %>%
#     as_tibble() %>%
#     filter(country_code == location_code) %>% 
#     pivot_longer(-c(country_code, name),
#                  names_to = "year",
#                  values_to = "SRB") %>% 
#     filter(year %in% floor(DatesOut))
#   
#   
#   years_srb <- dt$year
#   
#   SRB <- stats::setNames(dt$SRB, years_srb)
#   
#   if(length(SRB) == 0) { 
#     return(stats::setNames(rep(SRB_default, 3), DatesOut))
#   }
#   DatesOut    <- floor(DatesOut)
#   yrs_present <- DatesOut %in% years_srb
#   
#   if(any(!yrs_present)) {
#     
#     yrs_not_present <- mean(SRB[as.character(DatesOut[yrs_present])])
#     
#     yrs_not_present <- stats::setNames(rep(yrs_not_present, sum(!yrs_present)), DatesOut[!yrs_present])
#     SRB <- c(SRB, yrs_not_present)
#     
#   }
#   
#   SRB <- SRB[order(as.numeric(names(SRB)))]
#   return(SRB)
# }


# SRB        = NULL
# location   = "Argentina"
# DatesOut = 1950:2030

download_SRB <- function(SRB = NULL, 
                         location, 
                         DatesOut, 
                         verbose = TRUE) {
  
  SRB_default <- round((1 - 0.4886) / 0.4886, 3)
  
  # Check DatesOut
  if(length(DatesOut) < 1) {
    
    stop("DatesOut must contain at least one date.")
  }  
  
  # If SRB provided directly
  if (!is.null(SRB)) {
    
    SRB <- stats::setNames(rep(SRB, length.out = length(DatesOut)), DatesOut)
    
    return(SRB)
    
  }
  

  installed_wpp <- grep("^wpp\\d{4}$", rownames(installed.packages()), value = TRUE)
  
  if(length(installed_wpp) == 0) stop("No WPP package installed.")
  
  latest_wpp <- sort(installed_wpp, decreasing = TRUE)[1]
  
  data("sexRatio1", package = latest_wpp)
  
  location_code <- sexRatio1 %>%
    as_tibble() %>%
    subset(name %in% location, select = country_code) %>%
    unique() %>%
    as.numeric()
  
  if (is.na(location_code)) {
    if (verbose) cat(location, "not available in wpp. Using default SRB:", SRB_default, "\n")
    return(stats::setNames(rep(SRB_default, length(DatesOut)), DatesOut))
  }
  
  if(verbose) {
    cat(paste0("\nDownloading SRB for ", location, 
               " for years ", round(min(DatesOut),1), " to ", round(max(DatesOut),1), "\n"))
  }
  
  dt <- sexRatio1 %>%
    as_tibble() %>%
    filter(country_code == location_code) %>%
    pivot_longer(-c(country_code, name), names_to = "year", values_to = "SRB") %>%
    mutate(year = as.numeric(year)) %>%
    filter(year %in% floor(DatesOut))
  
  years_srb <- dt$year
  SRB <- stats::setNames(dt$SRB, years_srb)
  
  # Fill missing years with mean SRB
  DatesOut_floor <- floor(DatesOut)
  yrs_present <- DatesOut_floor %in% years_srb
  if(any(!yrs_present)) {
    mean_srb <- mean(SRB[as.character(DatesOut_floor[yrs_present])])
    SRB <- c(SRB, stats::setNames(rep(mean_srb, sum(!yrs_present)), DatesOut_floor[!yrs_present]))
  }
  
  SRB <- SRB[order(as.numeric(names(SRB)))]
  SRB <- stats::setNames(SRB, DatesOut)
  
  return(SRB)
}
