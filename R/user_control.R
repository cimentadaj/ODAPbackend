
# This script should contain the wrappers for graduate(), and smooth_age_5(), potentially blending them into a single wrapper with and age_out argument with possibilities "single", "abridged", and "five"

#' @param data_in a data.frame with columns Value and Age
#' @param fine_method the `method` argument of `graduate()`
#' @param rough_method the `method` argument of `smooth_age_5()`
#' @importFrom dplyr case_when
smooth_flexible <- function(data_in,
                            variable = "Deaths",
                            age_out = c("single","abridged","5-year"),
                            fine_method = c("auto","none","sprague", "beers(ord)", 
                                            "beers(mod)", "grabill", "pclm", "mono", "MAV",                              "uniform"),
                            rough_method = c("auto","none","Carrier-Farrag", "KKN", "Arriaga", 
                                         "United Nations", "Strong", "Zigzag", 
                                         "MAV","pclm"),
                            constraint_infant = TRUE){
  
  # ensure just one of each method is chosen. rough auto is compatible with a non-auto fine,
  # since we can always regroup to 5s. Likewise pclm rough is compatible with pclm fine; 
  # no pclm offsets in this implementation, and no explicit tail control.
  rough_method <- match.arg(rough_method, c("auto","none","Carrier-Farrag", "KKN", "Arriaga", 
                                "United Nations", "Strong", "Zigzag", "MAV","pclm"))
  fine_method <- match.arg(rough_method, c("auto","none","sprague", "beers(ord)", 
                                           "beers(mod)", "grabill", "pclm", 
                                           "mono", "MAV", "uniform"))
  
  # simplest case
  if (rough_method == "none" & fine_method == "none"){
    return(data_in)
  }
  
  # get variables
  value  <- data_in[, variable, drop = TRUE]
  age    <- data_in$Age
  age_in <- case_when(is_single(age) ~ "single",
                      is_abridged(age) ~ "abridged",
                      all((age %% 5) == 0) ~ "5-year",
                      TRUE ~ "undetermined")
  
  # some helper objects for flexible outbound ages
  age1   <- min(age):max(age)
  ageN   <- switch(age_out,
                   "single" = age1,
                   "5-year" = age1 - age1 %% 5,
                   "abridged" = calcAgeAbr(age1)
                   )
  # ------------------------#
  # I: Handle rough methods #
  # ------------------------#
  if (rough_method != "none"){
    # (1) the case of auto everything (verify arguments to pass)
    if (rough_method == "auto"){
      data1 <- graduate_auto(data_in, 
                                age_out = "single", 
                                variable = variable,
                                constraint_infant = constrain_infant)
      data_out[, variable, drop = TRUE]
      if (fine_method == "auto"){
        
      }
      
      # or do we regroup to 5 now? In this case, if we 
      data5 <- data_out |> 
        mutate(Age = Age - Age %% 5) |> 
        group_by(Age) |> 
        mutate(!!variable = sum(!!variable))
      
    } else {
      value5 <- smooth_age_5(Value = value, 
                   Age = age, 
                   method = rough_method)
      age5 <- names(value5) |> as.integer()
      
      data5 <- tibble(Age = age5, !!variable := value5)
    }
  }
  # ------------------------#
  # I: Handle fine methods  #
  # ------------------------#
  
  # I have an idea to keep the fine structure of our auto method while
  # constraining 5-year age groups to be whatever the above did. Odd,
  # I know, but maximally flexible?
  
  if (fine_method == "auto"){
    data_auto <- graduate_auto(data_in, 
                               age_out = "single", 
                               variable = variable,
                               constraint_infant = constrain_infant)
    
    
  }

  # plots <- plot_smooth_comparison(data_in, data_out)
  
  return(data_out)
}




