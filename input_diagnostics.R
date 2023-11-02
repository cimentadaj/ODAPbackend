# TODO: write function that evaluates age heaping on the input data,
# to be run on data-read in tab.
# it should take data frame in, as well as some default heaping diagnostics,
# it should return a small data.frame of

# Check the age hipping for 5 or 1 year data.
#' @description 
#' @param data data.frame. User file from the read_data command with the minimum data on Exposures, Death and Age. Data ca be both in 5 and 1 year age intervals
#' @param y chracter.Variable name for which the hipping should be checked `Deaths` or `Exposures`.
#' @return A data.frame with 2 columns `method` - the method used for age hipping evaluation and `result` - the resulting hipping measure
#' @importFrom stringr str_detect 
#' @examples1
#' \dontrun{
#' check_heapping_general(
#'     data = data,
#'     y = "Exposures")
#' }
#' 
check_heapping_general <- function(data, y) { 
  
  if(is_single(data$Age)) { 

    # go with 
bachi <- check_heaping_bachi(subset(data, select = y, drop = TRUE),  data$Age, OAG = TRUE)
myers <- check_heaping_myers(subset(data, select = y, drop = TRUE),  data$Age)

res <- data.frame(method = c("bachi", "myers"),
                  result = c(bachi, myers))

  } else { 

roughness <- check_heaping_roughness(subset(data, select = y, drop = TRUE), data$Age, ageMin = 30)
sawtooth  <- check_heaping_sawtooth( subset(data, select = y, drop = TRUE), data$Age, ageMin = 30)

res <- data.frame(method = c("roughness", "sawtooth"),
                  result = c(roughness, sawtooth))

  }

  return(res)

}

# add a user driven one with arguments
# Check the age hipping for 5 or 1 year data, but this time give user control over minimum and maximum evaluation age.
#' @description 
#' @param data data.frame. User file from the read_data command with the minimum data on Exposures, Death and Age. Data ca be both in 5 and 1 year age intervals
#' @param y chracter.Variable name for which the hipping should be checked `Deaths` or `Exposures`
#' @param ageMin numeric.The minimum age from which to do the hipping evaluation 
#' @param ageMax numeric.The maximum age from which to do the hipping evaluation 
#' @return A data.frame with 2 columns `method` - the method used for age hipping evaluation and `result` - the resulting hipping measure
#' @importFrom stringr str_detect 
#' @examples1
#' \dontrun{
#' check_heapping_general(
#'     data = data,
#'     y = "Exposures")
#' }
#' 
check_heapping_user <- function(data, y, ageMin, ageMax) { 
  
  if(is_single(data$Age)) { 
    
    # go with 
    bachi <- check_heaping_bachi(subset(data, select = y, drop = TRUE),  
                                 data$Age, 
                                 OAG = TRUE, 
                                 ageMin = ageMin,
                                 ageMax = ageMax,
                                 method = "orig")
    
    myers <- check_heaping_myers(subset(data, select = y, drop = TRUE),  
                                 data$Age,
                                 ageMin = ageMin,
                                 ageMax = ageMax)
    
    res <- data.frame(method = c("bachi", "myers"),
                      result = c(bachi, myers))
    
  } else { 
    
    roughness <- check_heaping_roughness(subset(data, select = y, drop = TRUE), 
                                         data$Age, 
                                         ageMin = ageMin,
                                         ageMax = ageMax)
    
    sawtooth  <- check_heaping_sawtooth( subset(data, select = y, drop = TRUE), 
                                         data$Age,
                                         ageMin = ageMin,
                                         ageMax = ageMax)
    
    res <- data.frame(method = c("roughness", "sawtooth"),
                      result = c(roughness, sawtooth))
    
  }
  
  return(res)
  
}