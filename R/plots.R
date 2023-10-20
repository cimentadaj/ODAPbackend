# This can be more modular, as in a separate call, please adjust so that it can just take these two
# data objects? Or whatever you think makes sense. This won't currently run because I changed the args
# and scoping, but just so you see this can also be modular

# TODO, return in list, make it a named list, where the names make the widget plot title
# Make a separate plot function for the input data with 3 plots:
# pop pyramid
# deaths pyramid
# input rates

make_figure <- function(data_out, data_in){
  # plot the results
  figure <- ggplot() + 
    geom_line(aes(x = Age, y = Mx_emp), linewidth = 0.8) + 
    geom_line(data = filter(data1, Age >= extrapFrom), aes(x = Age, y = nMx), lty = 2, col = "red", linewidth = 1) +
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
  return(lst(data   = out,
             figure = figure))
  
}

