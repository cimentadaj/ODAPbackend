#' @title `interpolate`
#' @description Several univariate interpolation methods. Methods allowed include nearest neighbor (`"nearest"`), Piecewise Cubic Hermite Interpolating Polynomial (`"pchip"`), cubic interpolation from four nearest neighbours (`"cubic"`), and `"spline"` interpolation using  assorted methods available in `stats::splinefun()`. `"linear"`,"`logarithmic`",`"geometric"`, and `"logit"` interpolation are also possible based on linear interpolations of y transforms.
#' @param data_in data.frame or tibble. Should contain numeric columns for x and y, although the names may be different than x and y, which you can control using the `xname` and `yname` arguments. x is the time variable, and y is the thing to interpolate. 
#' @details Logarithmic interpolation requires all values by greater than 0, geometric requires non-negative values, and logit requires all values to be between 0 and 1.
#' @param method character. options `"nearest"`, `"linear"`, `"logarithmic"`, `"geometric"`, `"logit"`, `"pchip"`, `"cubic"`, `"spline_fmm"`, `"spline_periodic"`, `"spline_natural"`, `"spline_monoH.FC"`, `"spline_hyman"`.
#' @param xout vector of x values we want interpolated values for. Default is the original x coordinates, meaning you get the same y values back unless you change this variable.
#' @param xname the name of the column holding the time variable
#' @param yname the name of the column holding the variable to interpolate
#' @param extrap logical, default `TRUE.` If `xout` exceeds the range of `x`
#' @importFrom signal interp1
#' @importFrom stats splinefun
#' @importFrom dplyr ungroup reframe mutate select
#' @importFrom tidyr unnest
#' @importFrom tidyselect all_of
#' @importFrom rlang sym !!
#' @importFrom purrr map2
#' @export
#' @examples
#' library(tibble)
#' x <- c(18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 
#'        63, 66, 69, 72)
#' y <- c(1, 2267, 7914, 14540, 21261, 28700, 36647, 43381, 49306, 54509, 
#' 58749, 61197, 62578, 63394, 63820, 63975, 64040, 64058, 64058)
#' xout = 10:80
#' data_in <- tibble(x = x, y = y)
#' \dontrun{
#' plot(data_in, xlim = c(10,80))
#' lines(interpolate(data_in, method = "pchip", xout = 10:80)$interp)
#' lines(interpolate(data_in, method = "logarithmic", xout = 10:80)$interp, col = "red")
#' lines(interpolate(data_in, method = "spline_monoH.FC", xout = 10:80)$interp,
#'       col = "blue", lty = "28")
#' }
#' 

interpolate <- function(data_in, 
                        xout = data_in[[xname]], 
                        method = c("nearest", "linear", "pchip", "cubic", "logarithmic","geometric","logit", "spline_fmm", "spline_periodic", "spline_natural", "spline_monoH.FC", "spline_hyman")[1], 
                        xname = "x", 
                        yname = "y",
                        extrap = TRUE) {
  
  stopifnot(
    length(data_in[[xname]]) == length(data_in[[yname]])
    )
  
  if (!(".id" %in% colnames(data_in))) {
    data_in <- data_in |>
      mutate(.id = "all")
  }
  
  if (grepl("spline", method)){
    
    method <- gsub(method, 
                   pattern = "spline_", 
                   replacement = "")
    
    out_df <- data_in |>
      ungroup() |>
      reframe(
        interp_func = list(splinefun(
          x = .data[[xname]],
          y = .data[[yname]],
          method = method,
          ties   = mean
        )),
        .by = all_of(c(".id"))
      ) |>
      mutate(
        !!xname := list(xout),  # Ensure `xout` is added as a list for proper alignment
        interp = map2(.data$interp_func, !!sym(xname), ~ .x(.y))  # Apply function to xout
      ) |>
      unnest(cols = c(!!sym(xname), "interp")) |>
      select(-c("interp_func"))
    
  } 
  
  if (method %in% c("linear", "geometric", 
                    "logarithmic", "logit")) {
    
    transform <- ifelse(method == "linear", 
                        "none", method)
    
    out_df <- data_in |>
      ungroup() |> 
      reframe(
        interp = interp_linear(
          x = .data[[xname]],
          y = .data[[yname]],
          xout = xout,
          transform = transform
        ), 
        .by = all_of(c(".id"))) |>
      mutate(!!xname := xout)
    
  }
  
  if (method %in% c("pchip","cubic","nearest")){
      
    out_df <- data_in |>
      ungroup() |> 
      reframe(
        interp = interp1(
          x = .data[[xname]],
                y = .data[[yname]],
                xi = xout,
                method = method,
                extrap = extrap
                ), 
        .by = all_of(c(".id"))) |>
      mutate(!!xname := xout)
    
  }
    
  # out_df <- tibble(!!xname := xout, !!yname := yout)
  return(out_df)
}

#' @title `interp_linear`
#' @description Perform a linear interpolation using `approxfun()`, potentially on a transform of the data. With no transform, we do a direct linear interpolation. For geometric/ logarithmic/ logit, we do a linear interpolation of the square root/ log/ logit of the data, then back transform the result.
#' @param x,y numeric vectors giving the coordinates of the points to be interpolated.
#' @param xout an optional set of numeric values specifying where interpolation is to take place.
#' @param transform character. Default `"none"`. Other options `"geometric"`,`"logarithmic"`,`"logit"`, if the data conforms with these, see details.
#' @param extrap logical, Default `TRUE`, do we allow extrapolation beyond the range of x? If so, it's constant at the nearest value.
#' @details For geometric transformations, `y` must be non-negative. For logarithmic, `y` must be greater than 0, and for logit we require `y` values between 0 and 1 (not inclusive). Direct linear extrapolation could extend into negative values, but the other options do not. Geometric interpolation may perform poorly if extrapolating beyond the tails. logit interpolation can also behave awkward in the tails. Logarithmic is behaves quite well for most demography applications.
#' @importFrom Hmisc approxExtrap
#' @export
#' @examples
#' 
#' x <- seq(18,36,by=3)
#' y <- c(100, 2267, 7914, 14540, 21261, 28700, 36647)
#' ylogit <- y / (max(y)+100)
#' 
#' xout <- 10:40
#' \dontrun{
#' plot(x,y,xlim=c(10,40),ylim=c(-5000,50000))
#' lines(xout, interp_linear(x,y,xout,"none"), col = "#202020", lty = "35")
#' lines(xout, interp_linear(x,y,xout,"geometric"), col = "#FF0000", lty = "53",lwd=2)
#' lines(xout, interp_linear(x,y,xout,"logarithmic"), col = "#0000FF", lty = "82",lwd=1)
#' }
#' \dontrun{
#'   plot(x,ylogit,xlim=c(10,40))
#'   lines(xout, interp_linear(x,y=ylogit,xout,"none"), col = "#202020", lty = "35")
#'   lines(xout, interp_linear(x,y=ylogit,xout,"logit"), col = "#FF0000", lty = "53",lwd=2)
#' }
#' 

interp_linear <- function(x,
                          y,
                          xout = x,
                          transform = c("none", "geometric", "logarithmic", "logit")[1],
                          extrap = TRUE){
  
  if (transform == "logarithmic"){
    stopifnot(all(y > 0))
    y <- log(y)
  }
  if (transform == "geometric"){
    y <- sqrt(y)
  }
  if (transform == "logit"){
    stopifnot(all(y >= 0 & y <= 1))
    y <- log(y/(1-y))
  }
  
  yout <- approxExtrap(x = x,
                    y = y, 
                    xout = xout,
                    method = "linear", 
                    rule = ifelse(extrap,2,1))$y
  
  if (transform == "logarithmic"){
    yout <- exp(yout)
  }
  if (transform == "geometric"){
    yout <- yout^2
  }
  if (transform == "logit"){
    yout <- exp(yout) / (1+exp(yout))
  }
  
  return(yout)
  
}

#' @title `check_interpolate`
#' @description Creates a plot of original data as points and interpolated result as line for each `.id`.
#' @param data_out a data.frame or tibble. The data.frame output of the `interpolate` function.
#' @param data_in a data.frame or tibble. The original data provided by user
#' @return A figure of original data as points and interpolated result as line for each `.id`
#' @importFrom dplyr mutate select summarise group_by
#' @importFrom scales pretty_breaks
#' @importFrom ggplot2 ggplot geom_point geom_line theme_minimal aes theme element_text scale_x_continuous scale_y_continuous 
#' @export
#' @examples
#' library(tibble)
#' x <- c(18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 
#'        63, 66, 69, 72)
#' y <- c(1, 2267, 7914, 14540, 21261, 28700, 36647, 43381, 49306, 54509, 
#' 58749, 61197, 62578, 63394, 63820, 63975, 64040, 64058, 64058)
#' xout = 10:80
#' data_in <- tibble(x = x, y = y)
#' data_out <- 
#' interpolate(data_in, method = "pchip", xout = 10:80)
#' check_interpolate(data_in, data_out)
#'

check_interpolate <- function(data_in, data_out) { 
  
  if (!(".id" %in% colnames(data_in))) {
    data_in <- data_in |>
      mutate(.id = "all")
  }
  
  ggplot() + 
    geom_point(data = data_in, aes(x = .data$x,
                                   y = .data$y,
                                   color = .data$.id)) +
    geom_line(data = data_out, aes(x = .data$x,
                                   y = .data$interp,
                                   color = .data$.id)) + 
    theme_minimal() + 
    theme(legend.position = "bottom",
          axis.text = element_text(color = "black")) + 
    scale_x_continuous(breaks = pretty_breaks()) + 
    scale_y_continuous(breaks = pretty_breaks())
}
