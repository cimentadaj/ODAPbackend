# This can be more modular, as in a separate call, please adjust so that it can just take these two
# data objects? Or whatever you think makes sense. This won't currently run because I changed the args
# and scoping, but just so you see this can also be modular


#' Plots a line graph of the log10 transformed empirical mortality rate (Mx) data and a fitted data from a chosen age.
#' @description 
#' @param data_in tibble. Empirical numeric Mx value to be plotted and and numeric Age values to be plotted.
#' @param data_out tibble. Modelled numeric Mx value to be plotted and and numeric Age values to be plotted.
#' @param extrapFrom numeric. extrapFrom an age interval from which the lifetable was extrapolated.
#' @return A line graph with black line corresponding to empirical Mx data and red line corresponding to modelled Mx data from the  chosen extrapFrom value. 
#' @importFrom ggplot2 ggplot geom_line scale_x_continuous scale_y_log10 theme_light geom_vline labs theme element_text
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

# make_figure(data_in = abridged_data, data_out = data_out, extrapFrom = 60)

# TODO, return in list, make it a named list, where the names make the widget plot title
# Make a separate plot function for the input data with 3 plots:
# pop pyramid
# deaths pyramid
# input rates

# a little data "simulation"
# data$sex        <- "Male" 
# data1           <- data
# data1$sex       <- "Female"
# data1$Exposures <- -data1$Exposures
# data1$Deaths    <- -data1$Deaths
# 
# z <- data %>% 
#   full_join(data1) %>% 
#   mutate(Deaths = ifelse(sex == "Female", Deaths + rpois(22, lambda = 50), Deaths)) # a bit of difference for females

# just a helper function for using both comma and abs for the axis labels
abs_and_comma <- function (x, ...) {
  
  format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)
  
}

# general pyramid function. WARNING contains some tidy evaluation
#' Plots a population or death pyramid
#' @description 
#' @param data tibble. Empirical data downloaded  with the `read_data` function 
#' @param y  character vector of length 1 indicating weather the `Exposures` or `Deaths` should be plotted.
#' @return A pyramid for either Deaths or Exposures
#' @importFrom ggplot2 ggplot geom_col scale_y_continuous coord_flip theme_light scale_fill_brewer theme theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @examples1
#' \dontrun{
#' pyramid(data = data, y = "Deaths")
#' }
pyramid <- function(data, y) {
  
  data %>%
    ggplot(aes(x = Age, y = (.data[[y]] / AgeInt), fill = sex, width = AgeInt)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(
      labels = abs_and_comma,
      limits = max(pull(mutate(z, new := .data[[y]] / AgeInt), new)) * c(-1,1)) +
    theme_light() +
    scale_fill_brewer(palette = "Dark2", guide = guide_legend(title = "Sex")) +
    theme(legend.position = "bottom",
          axis.text     = element_text(color = "black", size = 10),
          axis.title    = element_text(color = "black", size = 12),
          legend.text   = element_text(color = "black", size = 12),
          legend.title  = element_text(color = "black", size = 14),
          plot.subtitle = element_text(color = "black", size = 14)
    ) + 
    labs(subtitle = str_c("Pyramid of ", y, "."),
         y = y)
  }

# rates function
#' Plots a line graph of the log10 transformed empirical mortality rate (Mx).
#' @description 
#' @param data tibble. Empirical data downloaded  with the `read_data` function 
#' @return A linechart of log 10 scaled empirical `M(x)` values
#' @importFrom ggplot2 ggplot scale_y_log10 scale_y_continuous coord_flip theme_bw scale_fill_brewer theme theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @importFrom dplyr mutate
#' @examples1
#' \dontrun{
#' input_rates(data = data)
#' }
#' 
input_rates <- function(data) {

  data %>% 
    mutate(Mx_emp = Deaths / Exposures) %>%
    ggplot(aes(x = Age, y = Mx_emp, color = sex), linewidth = 0.8) + 
    geom_line() + 
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_log10(labels = label_log(digits = 2)) +
    theme_light() +
    labs(x = "Age",
         y = "nMx",
         subtitle = "Empirical Mx for a given age range on a log10 scale.")+
    theme(axis.text     = element_text(size = 10, color = "black"),
          plot.subtitle = element_text(size = 12, color = "black"))
  
}

# general hystogramm function. WARNING contains some tidy evaluation
#' Plots a population or death pyramid
#' @description 
#' @param data tibble. Empirical data downloaded  with the `read_data` function 
#' @param y  character vector of length 1 indicating weather the `Exposures` or `Deaths` should be plotted.
#' @return A histogramm for either Deaths or Exposures
#' @importFrom ggplot2 ggplot geom_col scale_y_continuous coord_flip theme_light scale_fill_brewer theme theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @examples1
#' \dontrun{
#' plot_histogram(data = data, y = "Deaths")
#' }
plot_histogram <- function(data, y) { 
  
  data %>% 
    ggplot(aes(x = Age, y = (.data[[y]] / AgeInt), fill = sex, width = AgeInt)) +
    geom_bar(stat = "identity") +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(
      labels = abs_and_comma,
      limits = max(pull(mutate(z, new := .data[[y]] / AgeInt), new)) * c(-1,1)) +
    theme_light() +
    scale_fill_brewer(palette = "Dark2", guide = guide_legend(title = "Sex")) +
    theme(legend.position = "bottom",
          axis.text     = element_text(color = "black", size = 10),
          axis.title    = element_text(color = "black", size = 12),
          legend.text   = element_text(color = "black", size = 12),
          legend.title  = element_text(color = "black", size = 14),
          plot.subtitle = element_text(color = "black", size = 14)
    ) + 
    labs(subtitle = str_c("Pyramid of ", y, "."),
         y = y)
}

# combine
#' Plots a line graph of the log10 transformed empirical mortality rate `M(x)`, population pyramid and death pyramid if the data contains information on two sex.
#' @description 
#' @param data tibble. Empirical data downloaded  with the `read_data` function 
#' @param plot_exposures logical indicates weather the population pyramid should be plotted, defaults to TRUE
#' @param plot_deaths logical indicates weather the death pyramid should be plotted, defaults to TRUE
#' @param plot_rates logical indicates weather the empirical `M(x)` should be plotted, defaults to TRUE
#' @return A named list with 3 elements: `Exposures` - population pyramid, `Deaths` - death pyramid and `Empirical Mx` - log 10 transformed empirical `M(x)` value
#' @importFrom ggplot2 ggplot scale_y_log10 scale_y_continuous coord_flip theme_bw scale_fill_brewer theme theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @importFrom dplyr mutate
#' @examples1
#' \dontrun{
#' initial_plot(data = data, plot_exposures = TRUE, plot_deaths = TRUE, plot_rates = TRUE)
#' }
initial_plot <- function(data, plot_exposures = TRUE, plot_deaths = TRUE, plot_rates = TRUE) {
  
  if(plot_exposures) { 
    
    Exposures <- pyramid(data = data, y = "Exposures")
    
  }
  
  if(plot_deaths) {
    
    Deaths <- pyramid(data = data, y = "Deaths")
    
  }
  
  if(plot_rates) { 
    
    `Empirical Mx` <- input_rates(data = data)
    
    }

  return(lst(Exposures, Deaths, `Empirical Mx`))

}


#' Plots a line graph of the log10 transformed empirical mortality rate `M(x)`, population histogram and death histogram if the data contains information on only one sex.
#' @description 
#' @param data tibble. Empirical data downloaded  with the `read_data` function 
#' @param plot_exposures logical indicates weather the population pyramid should be plotted, defaults to TRUE
#' @param plot_deaths logical indicates weather the death pyramid should be plotted, defaults to TRUE
#' @param plot_rates logical indicates weather the empirical `M(x)` should be plotted, defaults to TRUE
#' @return A named list with 3 elements: `Exposures` - population pyramid, `Deaths` - death pyramid and `Empirical Mx` - log 10 transformed empirical `M(x)` value
#' @importFrom ggplot2 ggplot scale_y_log10 scale_y_continuous coord_flip theme_bw scale_fill_brewer theme theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @importFrom dplyr mutate
#' @examples1
#' \dontrun{
#' initial_plot(data = data, plot_exposures = TRUE, plot_deaths = TRUE, plot_rates = TRUE)
#' }
initial_plot_single_sex <- function(data, plot_exposures = TRUE, plot_deaths = TRUE, plot_rates = TRUE) {
  
  if(plot_exposures) { 
    
    Exposures <- plot_histogram(data = data, y = "Exposures")
    
  }
  
  if(plot_deaths) {
    
    Deaths <- plot_histogram(data = data, y = "Deaths")
    
  }
  
  if(plot_rates) { 
    
    `Empirical Mx` <- input_rates(data = data)
    
  }
  
  return(lst(Exposures, Deaths, `Empirical Mx`))
  
}



# width bar to be equal to the ageInt and length is not the size is the size divided by age int
# assume there are 2 sexes AND create a bar chart for the case of one sex AND a checker if there is 1 or 2 sex

#' Plots the corresponding 3 graphics for single sex or for both sex depending on data 
#' @description 
#' @param data tibble. Empirical data downloaded  with the `read_data` function 
#' @param y  character vector of length 1 indicating weather the `Exposures` or `Deaths` should be plotted.
#' @param plot_exposures logical. Weather the exposures should be plotted
#' @param plot_deaths logical. Weather the Deaths should be plotted#' @param dplot_ratesata logical Empirical data downloaded  with the `read_data` function 
#' @param plot_rates logical. Weather the rates should be plotted#' @param dplot_ratesata logical Empirical data downloaded  with the `read_data` function 
#' @return A list with 3 corresponding plots for either one or two sex.
#' @importFrom ggplot2 ggplot geom_col scale_y_continuous coord_flip theme_light scale_fill_brewer theme theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @examples1
#' \dontrun{
#' plot_the_initial_data(data, plot_exposures = TRUE, plot_deaths = TRUE, plot_rates = TRUE)
#' }

plot_the_initial_data <- function(data, plot_exposures = TRUE, plot_deaths = TRUE, plot_rates = TRUE){
  
  if(length(unique(data$sex)) == 2) { 
  
  result <- initial_plot(data, plot_exposures = plot_exposures, plot_deaths = plot_deaths, plot_rates = plot_rates)
  
  } else { 
    
  result <- initial_plot_single_sex(data, plot_exposures = plot_exposures, plot_deaths = plot_deaths, plot_rates = plot_rates)
} 

  return(result)

}