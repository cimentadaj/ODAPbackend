#' @title `check_heaping_general`
#' @description Check the age heaping for 5 or 1 year data.
#' @param data data.frame. User file from the read_data command with the minimum data on `Exposures`, `Death` and `Age`. Data can be both in 5 and 1 year age intervals
#' @param y character.Variable name for which the heaping should be checked `Deaths` or `Exposures`.
#' @return A data.frame with 2 columns `method` - the method used for age heaping evaluation and `result` - the resulting heaping measure
#' @importFrom stringr str_detect 
#' @importFrom dplyr bind_rows left_join join_by
#' @importFrom rlang .data 
#' @importFrom DemoTools check_heaping_roughness check_heaping_bachi check_heaping_myers check_heaping_sawtooth
#' @export
#' @examples
#' \dontrun{
#' check_heaping_general(
#'     data = data,
#'     y = "Exposures")
#' }
#' 

check_heaping_general <- function(data, y) { 
  
  tbl <- tibble("level" = c(c("Highly accurate", "Fairly accurate", 
                            "Approximate", "Rough","Very rough")),
                color = c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026"))
  
  has_single <- is_single(data$Age)
  
  if(has_single) {
    
    # go with 
    bachi <- check_heaping_bachi(subset(data, select = y, drop = TRUE),  
                                 data$Age, 
                                 OAG = TRUE, 
                                 method = "pasex")
    
    myers <- check_heaping_myers(subset(data, select = y, drop = TRUE), data$Age)
    
    res <- tibble("age scale" = "single",
                  "method" = c("bachi", "myers"),
                      "result" = c(bachi, myers)) |> 
      mutate("level" = cut(.data$result, 
                  breaks = c(0,1,2,5,15,80),
                  labels = c("Highly accurate", "Fairly accurate", "Approximate",
                             "Rough","Very rough")) |> as.character()) |> 
      left_join(tbl, by = join_by("level"))
    
  } 
    tblr <- tibble("method" = "roughness",
                   "level"  = c(1.03))
  # 5-year methods
  roughness <- check_heaping_roughness(subset(data, select = y, drop = TRUE), data$Age, ageMin = 30)
  sawtooth  <- check_heaping_sawtooth( subset(data, select = y, drop = TRUE), data$Age, ageMin = 30)

  r <- cut(roughness, breaks = c(0,.1,.2,.5,1.5,10),
           labels = c("Highly accurate", "Fairly accurate", "Approximate",
                      "Rough","Very rough")) |> as.character()
  
  s <- cut(sawtooth, breaks = c(1,1.03,1.1,1.5,3,10),
           labels = c("Highly accurate", "Fairly accurate", "Approximate",
                      "Rough","Very rough")) |> as.character()
  
    res5 <- tibble('age scale' = "5-year",
                   "method"    = c("roughness", "sawtooth"),
                   "result"    = c(roughness, sawtooth),
                   "level"     = c(r, s)) |> 
      left_join(tbl, by = join_by("level"))
    
  if (has_single){
    
    out <- bind_rows(res, res5)
    
  } else {
    
    out <- res5
    
  }
  
  return(out)
  
}

#' @title `check_heaping_user`
#' @description Check the age heaping for 5 or 1 year data, but this time give user control over minimum and maximum evaluation age.
#' @param data data.frame. User file from the read_data command with the minimum data on Exposures, Death and Age. Data ca be both in 5 and 1 year age intervals
#' @param y chracter.Variable name for which the heaping should be checked `Deaths` or `Exposures`
#' @param ageMin numeric.The minimum age from which to do the heaping evaluation 
#' @param ageMax numeric.The maximum age from which to do the heaping evaluation 
#' @return A data.frame with 2 columns `method` - the method used for age heaping evaluation and `result` - the resulting heaping measure
#' @importFrom stringr str_detect 
#' @importFrom DemoTools check_heaping_roughness check_heaping_bachi check_heaping_myers check_heaping_sawtooth
#' @export
#' @examples
#' \dontrun{
#' check_heaping_general(
#'     data = data,
#'     y = "Exposures")
#' }
#' 

check_heaping_user <- function(data, y, ageMin, ageMax) { 
  
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
