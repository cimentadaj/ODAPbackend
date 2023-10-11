# This can be more modular, as in a separate call, please adjust so that it can just take these two
# data objects? Or whatever you think makes sense. This won't currently run because I changed the args
# and scoping, but just so you see this can also be modular


#' Plots a line graph of the log10 transformed empirical mortality rate (Mx) data and a fitted data from a chosen age.
#' @description 
#' @param data_in tibble. Empirical numeric Mx value to be plotted and and numeric Age values to be plotted.
#' @param data_out tibble. Modelled numeric Mx value to be plotted and and numeric Age values to be plotted.
#' @param extrapFrom numeric. extrapFrom an age interval from which the lifetable was extrapolated.
#' @return A line graph with black line corresponding to empirical Mx data and red line corresponding to modelled Mx data from the  chosen extrapFrom value. 
#' @importFrom ggplot ggplot geom_line scale_x_continuous scale_y_log10 theme_light geom_vline labs theme element_text
#' @importFrom scales label_log pretty_breaks
#' @importFrom dplyr filter
#' @examples1
#' \dontrun{
#' make_figure(data_in = data_in, data_out = data_out, extrapFrom = 60)
#' }
make_figure <- function(
                        data_in, # raw mx to plot
                        data_out, # the data from lt
                        extrapFrom) {
  # plot the results
  
  Mx_emp <- data_in$Deaths/ data_in$Exposures
  
  figure <- ggplot() + 
    geom_line(aes(x = data_in$Age, y = Mx_emp), linewidth = 0.8) + 
    geom_line(data = filter(data_out, Age >= extrapFrom), aes(x = Age, y = nMx), lty = 2, col = "red", linewidth = 1) +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_log10(labels = label_log(digits = 2)) +
    theme_light() +
    geom_vline(xintercept = extrapFrom, lty = 2)+
    labs(x = "Age",
         y = "nMx",
         subtitle = "The difference between the empirical Mx and the extrapolated values for a given age range on a log10 scale.")+
    theme(axis.text = element_text(color = "black"),
          plot.subtitle = element_text(size = 12, color = "black"))
  
  
  # return a list with both data and figure
  return(figure)
  
}



make_figure(data_in = abridged_data, data_out = data_out, extrapFrom = 60)
