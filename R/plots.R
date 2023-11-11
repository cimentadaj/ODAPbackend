
# TODO: 



# This can be more modular, as in a separate call, please adjust so that it can just take these two
# data objects? Or whatever you think makes sense. This won't currently run because I changed the args
# and scoping, but just so you see this can also be modular


#' make_figure
#' @description Plots a line graph of the log10 transformed empirical mortality rate (Mx) data and a fitted data from a chosen age. 
#' @param data_in tibble. Empirical numeric Mx value to be plotted and and numeric Age values to be plotted.
#' @param data_out tibble. Modelled numeric Mx value to be plotted and and numeric Age values to be plotted.
#' @param extrapFrom numeric. extrapFrom an age interval from which the lifetable was extrapolated.
#' @return A line graph with black line corresponding to empirical Mx data and red line corresponding to modelled Mx data from the  chosen extrapFrom value. 
#' @importFrom ggplot2 ggplot geom_line scale_x_continuous scale_y_log10 theme_light geom_vline labs theme element_text
#' @importFrom scales label_log pretty_breaks
#' @importFrom dplyr filter
#' @export
#' @examples
#' \dontrun{
#' plot_compare_rates(data_in = data_in, 
#'                    data_out = data_out, 
#'                    extrapFrom = 60)
#' }

# TODO: here we compare the input and fitted rates. Note, we also have potential abridging or graduation happening. If graduation has happened inside the lifetable, then I suggest showing both complete age schedules. And the title would read
# "Comparison of empirical nMx and lifetable nmx values", with caption (or annotation) reading "vertical line indicates extrapolation jump-off age"
plot_compare_rates <- function(
                        data_in, # raw mx to plot
                        data_out, # the data from lt
                        extrapFrom) {
  # plot the results
  
  Mx_emp <- data_in$Deaths/ data_in$Exposures
  
  figure <- ggplot() + 
    geom_line(aes(x = data_in$Age, y = Mx_emp), linewidth = 0.8) + 
    geom_line(data = filter(data_out, 
                            Age >= extrapFrom), 
              aes(x = Age, 
                  y = nMx), 
              lty = 2, 
              col = "red", 
              linewidth = 1) +
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

# TODO: create plot_lifetable() which creates a list of plots of lifetable functions# nMx, nqx, lx (indicating .25, .5, .75 quartiles as well as e0 using geom_vline(), dx (indicating .25, .5, .75 quartiles as well as e0 using geom_vline()). Quartiles can come from LifeIneq package in my github. We need to decide whether to represent age patterns as step or line functions. Maybe the user can decide?

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

# Just a helper function for using both comma and abs for the axis labels, No need for roxygen for this. Uses functions from base package only
abs_and_comma <- function (x, ...) {
  
  format(abs(x), ..., big.mark = ",", scientific = FALSE, trim = TRUE)
  
}

#' pyramid
#' @description Generate a population pyramid from the user data. WARNING contains some tidy evaluation 
#' @param data tibble. Empirical data downloaded  with the `read_data` function. Should contain the Exposures, Deaths 
#' @param y  character. This argument indicating weather the `Exposures` or `Deaths` should be plotted.
#' @return A pyramid for either Deaths or Exposures
#' @importFrom ggplot2 ggplot geom_col scale_y_continuous coord_flip theme_light scale_fill_brewer theme theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @export
#' @examples
#' \dontrun{
#' pyramid(data = mutate(data, sex = "Female"), y = "Deaths")
#' }
# TODO: careful with age bins; careful to filter only to sex %in% c("Male","Female"), since the data situation where we might plot a pyramid could also have a Total category.
# TODO: make an example in roxygen for 2-sex pyramid? 
# TODO: I didn't test result in roxygen, but I don't see the mechanism that would put Males on the left? Usually we'd make the values negative but then fix the legend to display absolute, which I see with abs_and_comma(), but not in the data itself. As in, before ggplot() is even called.
pyramid <- function(data, y) {
  
  data %>%
    ggplot(aes(x = Age, y = (.data[[y]] / AgeInt), fill = sex, width = AgeInt)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(
      labels = abs_and_comma,
      limits = max(pull(mutate(data, new := .data[[y]] / AgeInt), new)) * c(-1,1)) +
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

#' plot_input_rates
#' @description Plots a line graph of the log10 transformed empirical mortality rate (Mx).
#' @param data tibble. Empirical data downloaded  with the `read_data` function 
#' @return A linechart of log 10 scaled empirical `M(x)` values
#' @importFrom ggplot2 ggplot scale_y_log10 scale_y_continuous coord_flip theme_bw scale_fill_brewer theme theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @importFrom dplyr mutate
#' @export
#' @examples
#' \dontrun{
#' plot_input_rates(data = mutate(data, sex = "Female"))
#' }
#' 
# TODO: color = sex should only be used if there are >1 sex categories.
# Or at least the fill legend suppressed if there is only one sex. I modified the code for this, can you check it?
plot_input_rates <- function(data) {
 
  if (any(colnames(data) == "sex")){
    p <-  data %>% 
      mutate(Mx_emp = Deaths / Exposures) %>%
      ggplot(aes(x = Age, y = Mx_emp, color = sex), linewidth = 0.8) 
  } else {
    p <-  data %>% 
      mutate(Mx_emp = Deaths / Exposures) %>%
      ggplot(aes(x = Age, y = Mx_emp), linewidth = 0.8) 
  }
  
    p + 
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

#' plot_histogram
#' @description Plots a histogram of population or death, depending on the user choice.
#' @param data tibble. Empirical data downloaded  with the `read_data` function 
#' @param y character. This argument indicats weather the `Exposures` or `Deaths` should be plotted.
#' @return A histogramm for either Deaths or Exposures
#' @importFrom ggplot2 ggplot geom_col scale_y_continuous coord_flip theme_light scale_fill_brewer theme theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @export
#' @examples
#' \dontrun{
#' plot_histogram(data = mutate(data, sex = "Female"), y = "Deaths")
#' }
# TODO: (repeated elsewhere too): age bin categories should be precise and not centered on the midpoints.
# TODO: no need to map fill to sex for a single-sex plot? Or at least drop the legend? I removed sex mapping, only called for single sex 
plot_histogram <- function(data, y) { 
  
  data %>% 
    ggplot(aes(x = Age, y = (.data[[y]] / AgeInt), width = AgeInt)) +
    geom_bar(stat = "identity") +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(
      labels = abs_and_comma,
      limits = max(pull(mutate(data, new := .data[[y]] / AgeInt), new)) * c(-1,1)) +
    theme_light() +
    scale_fill_brewer(palette = "Dark2", guide = guide_legend(title = "Sex")) +
    theme(legend.position = "bottom",
          axis.text     = element_text(color = "black", size = 10),
          axis.title    = element_text(color = "black", size = 12),
          legend.text   = element_text(color = "black", size = 12),
          legend.title  = element_text(color = "black", size = 14),
          plot.subtitle = element_text(color = "black", size = 14)
    ) + 
    labs(subtitle = str_c("Age histogram of ", y, "."),
         y = y)
}

#' plot_initial_two_sex
#' @description Plots a line graph of the log10 transformed empirical mortality rate `M(x)`, population pyramid and death pyramid if the data contains information on two sex.
#' @param data tibble. Empirical data downloaded  with the `read_data` function 
#' @param plot_exposures logical indicates weather the population pyramid should be plotted, defaults to TRUE
#' @param plot_deaths logical indicates weather the death pyramid should be plotted, defaults to TRUE
#' @param plot_rates logical indicates weather the empirical `M(x)` should be plotted, defaults to TRUE
#' @return A named list with 3 elements: `Exposures` - population pyramid, `Deaths` - death pyramid and `Empirical Mx` - log 10 transformed empirical `M(x)` value
#' @importFrom ggplot2 ggplot scale_y_log10 scale_y_continuous coord_flip theme_bw scale_fill_brewer theme theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @importFrom dplyr mutate
#' @export
#' @examples
#' \dontrun{
#' plot_initial_two_sex(data = mutate(data, 
#'                                    sex = "Female"), 
#'                      plot_exposures = TRUE, 
#'                      plot_deaths = TRUE, 
#'                      plot_rates = TRUE)
#' }
# TODO: nice this works also on single-sex data, but can we make a working example for 2-sexes in the roxygen? Ensure it's just Male and Female. Total only allowed if just one sex is present. Right?
plot_initial_two_sex <- function(data, 
                                 plot_exposures = TRUE, 
                                 plot_deaths = TRUE, plot_rates = TRUE) {
  
  if(plot_exposures) { 
    
    Exposures <- pyramid(data = data, y = "Exposures")
    
  }
  
  if(plot_deaths) {
    
    Deaths <- pyramid(data = data, y = "Deaths")
    
  }
  
  if(plot_rates) { 
    
    `Empirical Mx` <- plot_input_rates(data = data)
    
    }

  return(lst(Exposures, Deaths, `Empirical Mx`))

}

#' plot_initial_single_sex
#' @description Plots a line graph of the log10 transformed empirical mortality rate `M(x)`, population histogram and death histogram if the data contains information on only one sex.
#' @param data tibble. Empirical data downloaded  with the `read_data` function 
#' @param plot_exposures logical indicates weather the population pyramid should be plotted, defaults to TRUE
#' @param plot_deaths logical indicates weather the death pyramid should be plotted, defaults to TRUE
#' @param plot_rates logical indicates weather the empirical `M(x)` should be plotted, defaults to TRUE
#' @return A named list with 3 elements: `Exposures` - population pyramid, `Deaths` - death pyramid and `Empirical Mx` - log 10 transformed empirical `M(x)` value
#' @importFrom ggplot2 ggplot scale_y_log10 scale_y_continuous coord_flip theme_bw scale_fill_brewer theme theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @importFrom dplyr mutate
#' @export
#' @examples
#' \dontrun{
#' plot_initial_single_sex(data = mutate(data, 
#'                                       sex = "Female"), 
#'                         plot_exposures = TRUE, 
#'                         plot_deaths = TRUE, 
#'                         plot_rates = TRUE)
#' }
plot_initial_single_sex <- function(data, 
                                    plot_exposures = TRUE, 
                                    plot_deaths = TRUE, plot_rates = TRUE) {
  
  if(plot_exposures) { 
    Exposures <- plot_histogram(data = data, y = "Exposures")
    
  }
  
  if(plot_deaths) {
    Deaths <- plot_histogram(data = data, y = "Deaths")
    
  }
  
  if(plot_rates) { 
    
    `Empirical Mx` <- plot_input_rates(data = data)
    
  }
  
  return(lst(Exposures, Deaths, `Empirical Mx`))
  
}



# width bar to be equal to the ageInt and length is not the size is the size divided by age int
# assume there are 2 sexes AND create a bar chart for the case of one sex AND a checker if there is 1 or 2 sex

#' plot_initial_data
#' @description Plots the corresponding 3 graphics for single sex or for both sex depending on data provided by the user.
#' @param data tibble. Empirical data downloaded  with the `read_data` function 
#' @param plot_exposures logical. Weather the exposures should be plotted
#' @param plot_deaths logical. Weather the Deaths should be plotted#' @param dplot_ratesata logical Empirical data downloaded  with the `read_data` function 
#' @param plot_rates logical. Weather the rates should be plotted#' @param dplot_ratesata logical Empirical data downloaded  with the `read_data` function 
#' @return A list with 3 corresponding plots for either one or two sex.
#' @importFrom ggplot2 ggplot geom_col scale_y_continuous coord_flip theme_light scale_fill_brewer theme theme element_text guide_legend
#' @importFrom scales label_log pretty_breaks
#' @export
#' @examples
#' \dontrun{
#' plot_initial_data(mutate(data, sex = "Female"), 
#'                   plot_exposures = TRUE, 
#'                   plot_deaths = TRUE, 
#'                   plot_rates = TRUE)
#' }
# TODO: here, we check for 2 sex categories. I can imagine a dataset with Male, Female, Total, for which we should discard Total. I like that currently, we are flexible with respect to sex names, so I don't know what's more important. For line plots it's not a problem, but for pyramids it is a problem. I made a modification for this, can you check it?
plot_initial_data <- function(data, 
                              plot_exposures = TRUE, 
                              plot_deaths = TRUE, 
                              plot_rates = TRUE){
  if ("sex" %in% colnames(data)){
   sexes <- data$sex |> unique()
   if (length(sexes) >= 2){
    if (length(sexes) > 2){
      data <- 
        data |> 
        filter(sex %in% c("Male","Female"))
    }
    result <- plot_initial_two_sex(data, 
                                   plot_exposures = plot_exposures, 
                                   plot_deaths = plot_deaths, 
                                   plot_rates = plot_rates)
   }
  } else { 
    
  result <- plot_initial_single_sex(data, plot_exposures = plot_exposures, plot_deaths = plot_deaths, plot_rates = plot_rates)
} 

  return(result)

}