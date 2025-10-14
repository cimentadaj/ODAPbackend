#' @title Cohort-Component Population Reconstruction with Single-Year Ages
#'
#' @description
#' Estimates the base population by age and sex using the cohort-component
#' reverse survival method with **single-year ages**. The function reconstructs
#' births from observed female populations of reproductive age, age-specific
#' fertility rates (ASFR), survival ratios, and the sex ratio at birth (SRB).
#' Births are then distributed by sex and survived to ages 0–9, replacing
#' reported counts in those ages.
#'
#' @details
#' This function is analogous to [basepop_five()], but works with **single-year**
#' age groups instead of 5-year age groups. Female exposures aged 15–49 are
#' reverse-survived one year to estimate births across three anchor dates
#' (`refDate - c(0.5, 2.5, 7.5)`). Births are then split by sex using the SRB
#' and survived to obtain reconstructed populations for ages 0–9. Older ages
#' remain unchanged.
#'
#' @param location Optional. Country or region name/ISO code. Used if inputs are
#'   to be automatically downloaded from WPP.
#' @param refDate Numeric or `Date`. Reference date of the population count (e.g.,
#'   census year and month, converted internally to decimal date).
#' @param Age Integer vector. Lower bounds of single-year ages (if `NULL`, inferred
#'   from input names or length of `Females_single`).
#' @param Females_single Numeric vector. Female population counts by single year
#'   of age at `refDate`.
#' @param Males_single Optional numeric vector. Male population counts by single year
#'   of age at `refDate`.
#' @param nLxFemale,nLxMale Life table person-years lived by age and sex. Can be
#'   provided directly or downloaded if `location` is supplied.
#' @param nLxDatesIn Numeric vector of input dates corresponding to `nLxFemale`
#'   and `nLxMale`. Defaults to `refDate - c(0.5, 9.5)`.
#' @param AsfrMat Matrix or data.frame of age-specific fertility rates (ASFR),
#'   with ages 15–49 in rows and dates in columns.
#' @param AsfrDatesIn Numeric vector of input dates for `AsfrMat`. Defaults to
#'   `refDate - c(0.5, 9.5)`.
#' @param SRB Optional numeric vector of sex ratios at birth (males per female).
#'   If `NULL`, values are downloaded when `location` is provided.
#' @param SRBDatesIn Numeric vector of input dates for `SRB`. Defaults to the
#'   three anchor dates `refDate - c(0.5, 2.5, 7.5)`.
#' @param radix Life table radix (default = 1). If `NULL`, inferred from `nLx`.
#' @param verbose Logical (default = `TRUE`). Print assumptions and progress.
#' @param ... Further arguments passed to [interp()].
#'
#' @return A list with components:
#' \describe{
#'   \item{Females_adjusted}{Numeric vector of reconstructed female counts (ages 0–9 adjusted).}
#'   \item{Males_adjusted}{Numeric vector of reconstructed male counts (ages 0–9 adjusted).}
#'   \item{Females_single}{Original female counts (input).}
#'   \item{Males_single}{Original male counts (input).}
#'   \item{nLxf}{Interpolated female life table values.}
#'   \item{nLxm}{Interpolated male life table values.}
#'   \item{Asfr}{Interpolated ASFR by age and date.}
#'   \item{Exposure_female}{Estimated female exposures aged 15–49 at anchor dates.}
#'   \item{Bt}{Estimated total births at each anchor date.}
#'   \item{SRB}{Sex ratio at birth used in the reconstruction.}
#'   \item{Age}{Age vector used.}
#' }
#'
#' @seealso [basepop_five()]
#'
#' @examples
#' \dontrun{
#' pop <- basepop_single(
#'   location = "Sweden",
#'   refDate = 2000,
#'   Females_single = SwedenFemales,
#'   Males_single   = SwedenMales
#' )
#' }
#'
#' @export



# example setup to run the function
method     = "linear"
refDate  <- 1986
location <- "Brazil"
pop_female_single <- fertestr::FetchPopWpp2019(location,
                                               refDate,
                                               ages = 0:100,
                                               sex = "female")

Age <- pop_female_single$ages

Males_single     <- pop_female_single$pop
Females_single   <- fertestr::FetchPopWpp2019(location,
                                              refDate,
                                              ages = 0:100,
                                              sex = "male")$pop
nLxFemale = NULL
nLxMale = NULL
nLxDatesIn = NULL
AsfrMat = NULL
AsfrDatesIn = NULL
SRB = NULL
SRBDatesIn = NULL
radix = 100000
verbose = TRUE

# !!!!!!!!!!!!!!!! NOTE: Two versions of the function
# Currenly the second one that is provided below is used
# !!!!!!!!!!!!!!!! IMPORTANT: I have not checked this function for robust work
# just with minimal examples
# If you like the implementation, I will make the final version in 1-2 days


# basepop_single <- function(location = NULL,
#                            refDate,
#                            Age = NULL,
#                            Females_single,
#                            Males_single = NULL,
#                            nLxFemale = NULL,
#                            nLxMale = NULL,
#                            nLxDatesIn = NULL,
#                            AsfrMat = NULL,
#                            AsfrDatesIn = NULL,
#                            ...,
#                            SRB = NULL,
#                            SRBDatesIn = NULL,
#                            radix = 100000,
#                            verbose = TRUE) {
#   
#   options(basepop_verbose = verbose)
#   on.exit(options(basepop_verbose = NULL))
#   refDate <- dec.date(refDate)
#   
#   # 1) Age setup
#   if (!is.null(Age)) {
#     
#     stopifnot(is_single(Age)) # user must define this helper: check contiguous 0,1,2,...
#     stopifnot(length(Age) == length(Females_single))
#     
#   } else {
#     
#     if (!is.null(names(Females_single))) {
#       Age <- as.integer(names(Females_single))
#     } else {
#       if (verbose)
#         cat("Assuming age groups are single years starting at 0\n")
#       Age <- 0:(length(Females_single) - 1)
#     }
#   }
#   
#   
#   # 2) Default input dates
#   if (is.null(nLxDatesIn)) {
#     
#     nLxDatesIn <- refDate - c(0.5, 9.5)
#     
#     if (verbose) {
#       cat(
#         "Assuming the two prior dates for the nLx matrix to be: ",
#         paste0(nLxDatesIn, collapse = ", "),
#         "\n"
#       )
#     }
#   }
#   
#   
#   if (is.null(AsfrDatesIn)) {
#     AsfrDatesIn <- refDate - c(0.5, 9.5)
#     if (verbose) {
#       cat(
#         "Assuming the two prior dates for the Asfr matrix to be: ",
#         paste0(AsfrDatesIn, collapse = ", "),
#         "\n"
#       )
#     }
#   }
#   
#   # 3) Assign names
#   names(Females_single) <- Age
#   names(Males_single)   <- Age
#   
#   # 4) Download or prepare nLx, ASFR, SRB
#   nLxFemale <- download_Lx(
#     nLx        = nLxFemale,
#     location   = location,
#     gender     = "female",
#     nLxDatesIn = nLxDatesIn
#   )
#   
#   nLxMale   <- download_Lx(
#     nLx        = nLxMale,
#     location   = location,
#     gender     = "male",
#     nLxDatesIn = nLxDatesIn
#   )
#   
#   if(is.null(radix)) {
#     radix <- lt_infer_radix_from_1L0(nLxMale[1, 1])
#     
#     if (verbose)
#       cat("Setting radix to lx = ",
#           radix,
#           ". Can be overwritten with `radix` argument\n")
#     
#   }
#   
#   # downloadAsfr(Asfrmat     = NULL,
#   #              location    = location,
#   #              AsfrDatesIn = AsfrDatesIn)
#   
#   # check this maybe wrong
#   # the only one place
#   AsfrMat <- download_Asfr(AsfrMat,
#                            location    = location,
#                            AsfrDatesIn = AsfrDatesIn) %>%
#     filter(between(age, 15, 49))
#   
#   # anchor dates: ages 0-9
#   DatesOut <- refDate - ((0:9) + 0.5)
#   
#   SRBDatesIn <- if(!is.null(SRBDatesIn)) {
#     
#     SRBDatesIn
#     
#   } else {
#     
#     DatesOut
#     
#   }
#   
#   SRB <- download_SRB(SRB, 
#                       location, 
#                       DatesOut = SRBDatesIn, 
#                       verbose  = verbose)
#   
#   # 5) Checks
#   AllArgs <- as.list(environment())
#   
#   # ArgsCheck(AllArgs)
#   # 
#   lower_bound <- abs(min(nLxDatesIn) - min(DatesOut))
#   upper_bound <- abs(max(nLxDatesIn) - max(DatesOut))
#   
#   if (lower_bound > 5 || upper_bound > 5) {
#     stop("nLxDatesIn extrapolation > 5 years not allowed")
#   }
#   
#   nLxf <- interp(nLxFemale[,-c(1:4)], 
#                  datesIn  = nLxDatesIn, 
#                  datesOut = DatesOut,
#                  ...) # ... everywhere
#   
#   nLxm <- interp(nLxMale[,-c(1:4)], 
#                  datesIn = nLxDatesIn, 
#                  datesOut = DatesOut,
#                  ...)
#   
#   lower_bound <- abs(min(AsfrDatesIn) - min(DatesOut))
#   upper_bound <- abs(max(AsfrDatesIn) - max(DatesOut))
#   
#   if (lower_bound > 5 || upper_bound > 5) {
#     stop("AsfrDatesIn extrapolation > 5 years not allowed")
#   }
#   
#   Asfr <- interp(AsfrMat[,-c(1:2, 5)], 
#                  datesIn = AsfrDatesIn, 
#                  datesOut = DatesOut,
#                  ...)
#   
#   # 6) Female exposures by reverse survival and interpolation
#   ages_repro <- 15:49
#   Fcurrent <- Females_single[as.character(ages_repro)]
#   
#   # reverse survival one-year steps
#   Ft_minus_1 <- Fcurrent
#   
#   for(a in (16:49)) {
#     
#     Ft_minus_1[as.character(a - 1)] <- Fcurrent[as.character(a)] *
#       nLxf[as.character(a - 1), which(DatesOut == (refDate - 0.5))] /
#       nLxf[as.character(a), which(DatesOut == (refDate - 0.5))]
#     
#   }
#   
#   # Not sure which version to use 
#   # exposures: interpolate between F(t) and F(t-1)
#   # fExpos <- sapply(DatesOut, function(s) {
#   # 
#   #   weight <- (refDate - s) / 1  # always 0.5 at midpoints
#   # 
#   #   (1 - weight) * Fcurrent + weight * Ft_minus_1[names(Fcurrent)]
#   # })
#   # colnames(fExpos) <- as.character(DatesOut)
#   # 
#   
#   # I like this weights more
#   fExpos <- sapply(DatesOut, function(s) {
#     weight <- (refDate - s) / (refDate - min(DatesOut))  # normalized to [0,1]
#     (1 - weight) * Fcurrent + weight * Ft_minus_1[names(Fcurrent)]
#   })
#   colnames(fExpos) <- as.character(DatesOut)
#   
#   
#   # 7) Births at each anchor
#   Bt <- colSums(fExpos * Asfr)
#   
#   
#   # 8) Split by sex and compute adjusted counts
#   PF          <- 1 / (SRB + 1)
#   Females_out <- Females_single
#   Males_out   <- Males_single
#   
#   
#   for (x in 0:9) {
#     
#     d <- x + 1   # match age with DatesOut index
#     Females_out[as.character(x)] <- Bt[d] * PF[d] * nLxf[x + 1, d] / radix
#     Males_out[as.character(x)]   <- Bt[d] * (1 - PF[d]) * nLxm[x + 1, d] / radix
#     
#   }
#   
#   
#   age_groups <- list(
#     "0"    = 1,   # Bt[1], PF[1], nLx[,1]
#     "1:4"  = 2,   # Bt[2], PF[2], nLx[,2]
#     "5:9"  = 3    # Bt[3], PF[3], nLx[,3]
#   )
#   
#   for (group in names(age_groups)) {
#     d    <- age_groups[[group]]        # anchor index
#     ages <- eval(parse(text = group))  # expand e.g. "1:4" → 1 2 3 4
#     
#     for (x in ages) {
#       Females_out[as.character(x)] <- Bt[d] * PF[d] * nLxf[x + 1, d] / radix
#       
#       Males_out[as.character(x)]   <- Bt[d] * (1 - PF[d]) * nLxm[x + 1, d] / radix
#     }
#   }
#   
#   
#   
#   # the figuire will look something like this
#   # tibble(new = Females_out,
#   #        old = Females_single,
#   #        Age = Age) %>%
#   #   filter(between(Age, 0, 10)) %>%
#   #   ggplot(aes(x = Age)) +
#   #   geom_line(aes(y = new), color = "red") +
#   #   geom_line(aes(y = old), color = "black") +
#   #   theme_minimal() +
#   #   theme(legend.position = "bottom")
#   
#   list(
#     Females_adjusted = Females_out,
#     Males_adjusted   = Males_out,
#     Females_single   = Females_single,
#     Males_single     = Males_single,
#     nLxf             = nLxf,
#     nLxm             = nLxm,
#     Asfr             = Asfr,
#     Exposure_female  = fExpos,
#     Bt               = Bt,
#     SRB              = SRB,
#     Age              = Age
#   )
# }


# ------------------------------------------------------------------------- #
basepop_single <- function(location = NULL,
                           refDate,
                           Age = NULL,
                           Females_single,
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
  
  options(basepop_verbose = verbose)
  on.exit(options(basepop_verbose = NULL))
  refDate <- dec.date(refDate)
  
  ## 1) Age setup
  if (!is.null(Age)) {
    stopifnot(is_single(Age))
    stopifnot(length(Age) == length(Females_single))
  } else {
    if (!is.null(names(Females_single))) {
      Age <- as.integer(names(Females_single))
    } else {
      if (verbose) cat("Assuming age groups are single years starting at 0\n")
      Age <- 0:(length(Females_single) - 1)
    }
  }
  
  ## 2) Default input dates
  if (is.null(nLxDatesIn)) {
    nLxDatesIn <- refDate - c(0.5, 9.5)
    if (verbose) cat("Assuming nLx input dates: ", paste0(nLxDatesIn, collapse = ", "), "\n")
  }
  
  if (is.null(AsfrDatesIn)) {
    AsfrDatesIn <- refDate - c(0.5, 9.5)
    if (verbose) cat("Assuming ASFR input dates: ", paste0(AsfrDatesIn, collapse = ", "), "\n")
  }
  
  ## 3) Assign names
  names(Females_single) <- Age
  if (!is.null(Males_single)) names(Males_single) <- Age
  
  ## 4) Download or prepare inputs
  nLxFemale <- download_Lx(nLxFemale, location, "female", nLxDatesIn)
  nLxMale   <- download_Lx(nLxMale, location, "male",   nLxDatesIn)
  
  if (is.null(radix)) {
    radix <- lt_infer_radix_from_1L0(nLxMale[1, 1])
    if (verbose) cat("Setting radix to lx = ", radix, "\n")
  }
  
  AsfrMat <- download_Asfr(AsfrMat, location, AsfrDatesIn)
  
  ## Anchor dates: 3 points like basepop_five
  DatesOut <- refDate - c(0.5, 2.5, 7.5)
  
  if (is.null(SRBDatesIn)) SRBDatesIn <- DatesOut
  SRB <- download_SRB(SRB, location, SRBDatesIn, verbose)
  
  ## 5) Checks
  if (abs(min(nLxDatesIn) - min(DatesOut)) > 5 ||
      abs(max(nLxDatesIn) - max(DatesOut)) > 5) {
    stop("nLxDatesIn extrapolation > 5 years not allowed")
  }
  
  nLxf <- interp(nLxFemale[, -c(1:4)], datesIn = nLxDatesIn, datesOut = DatesOut)
  nLxm <- interp(nLxMale[, -c(1:4)],   datesIn = nLxDatesIn, datesOut = DatesOut)
  
  if (abs(min(AsfrDatesIn) - min(DatesOut)) > 5 ||
      abs(max(AsfrDatesIn) - max(DatesOut)) > 5) {
    stop("AsfrDatesIn extrapolation > 5 years not allowed")
  }
  
  AsfrMat <- AsfrMat %>%
    filter(age %in% 15:49) %>%
    select(-c(country_code, name, age))
  
  Asfr <- interp(AsfrMat, datesIn = AsfrDatesIn, datesOut = DatesOut)
  
  ## 6) Female exposures
  ages_repro <- 15:49
  Fcurrent <- Females_single[as.character(ages_repro)]
  
  # one-year reverse survival
  Ft_minus_1 <- Fcurrent
  for (a in 16:49) {
    Ft_minus_1[as.character(a - 1)] <- Fcurrent[as.character(a)] *
      nLxf[as.character(a - 1), 1] /
      nLxf[as.character(a), 1]
  }
  
  # exposures: linear interpolation between F(t) and F(t-1)
  fExpos <- sapply(DatesOut, function(s) {
    weight <- (refDate - s) / 1
    (1 - weight) * Fcurrent + weight * Ft_minus_1[names(Fcurrent)]
  })
  colnames(fExpos) <- as.character(DatesOut)
  
  ## 7) Births
  Bt <- colSums(fExpos * Asfr)
  
  ## 8) Split by sex and adjust ages 0–9
  PF <- 1 / (SRB + 1)
  Females_out <- Females_single
  Males_out   <- Males_single
  
  age_groups <- list(
    "0"   = 1,  # anchor 1
    "1:4" = 2,  # anchor 2
    "5:9" = 3   # anchor 3
  )
  
  for (group in names(age_groups)) {
    d <- age_groups[[group]]
    ages <- eval(parse(text = group))
    for (x in ages) {
      Females_out[as.character(x)] <- Bt[d] * PF[d] * nLxf[x + 1, d] / radix
      Males_out[as.character(x)]   <- Bt[d] * (1 - PF[d]) * nLxm[x + 1, d] / radix
    }
  }
  
  
  # example figure
  #tibble(new = Females_out,
  #        old = Females_single,
  #        Age = Age) %>%
  #   filter(between(Age, 0, 10)) %>%
  #   ggplot(aes(x = Age)) +
  #   geom_line(aes(y = new), color = "red") +
  #   geom_line(aes(y = old), color = "black") +
  #   theme_minimal() +
  #   theme(legend.position = "bottom")
  
  ## 9) Return
  list(
    Females_adjusted = Females_out,
    Males_adjusted   = Males_out,
    Females_single   = Females_single,
    Males_single     = Males_single,
    nLxf             = nLxf,
    nLxm             = nLxm,
    Asfr             = Asfr,
    Exposure_female  = fExpos,
    Bt               = Bt,
    SRB              = SRB,
    Age              = Age
  )
}
