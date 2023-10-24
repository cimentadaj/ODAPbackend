# TODO: write function that evaluates age heaping on the input data,
# to be run on data-read in tab.
# it should take data frame in, as well as some default heaping diagnostics,
# it should return a small data.frame of


check_heapping_general <- function(data) { 
  
  if(is_single(data$Age)) { 

bachi <- check_heaping_bachi(data$Exposures,  data$Age,  ageMin = min(data$Age), ageMax = max(data$Age),
                             method = "orig", OAG = FALSE)
myers <- check_heaping_myers(data$Exposures,  data$Age,  ageMin = min(data$Age), ageMax = max(data$Age),
                             method = "orig")
res <- data.frame(method = c("bachi", "myers"),
result = c(bachi, myers))

  } else { 

roughness <- check_heaping_roughness(data$Exposures, data$Age, ageMin = min(data$Age), ageMax = max(data$Age))
sawtooth  <- check_heaping_sawtooth( data$Exposures, data$Age, ageMin = min(data$Age), ageMax = max(data$Age))

res <- data.frame(method = c("roughness", "sawtooth"),
                  result = c(roughness, sawtooth))

  }

  return(res)

}
