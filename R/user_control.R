
# This script should contain the wrappers for graduate(), and smooth_age_5(), potentially blending them into a single wrapper with and age_out argument with possibilities "single", "abridged", and "five"

#' @param data_in a data.frame with columns Value and Age
#' @param fine_method the `method` argument of `graduate()`
#' @param rough_method the `method` argument of `smooth_age_5()`
smooth_flexible <- function(data_in,
                            age_out = c("single","abridged","five"),
                            fine_method = c("sprague", "beers(ord)", 
                                            "beers(mod)", "grabill", "pclm", "mono", "MAV",                              "uniform"),
                            rough_method = c("Carrier-Farrag", "KKN", "Arriaga", 
                                         "United Nations", "Strong", "Zigzag", 
                                         "MAV","pclm")){
  
  
  plots <- plot_smooth_comparison(data_in, data_out)
  
  return(data_out)
}




