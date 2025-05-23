% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolation.R
\name{check_interpolate}
\alias{check_interpolate}
\title{\code{check_interpolate}}
\usage{
check_interpolate(data_in, data_out)
}
\arguments{
\item{data_in}{a data.frame or tibble. The original data provided by user}

\item{data_out}{a data.frame or tibble. The data.frame output of the \code{interpolate} function.}
}
\value{
A figure of original data as points and interpolated result as line for each \code{.id}
}
\description{
Creates a plot of original data as points and interpolated result as line for each \code{.id}.
}
\examples{
library(tibble)
x <- c(18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 
       63, 66, 69, 72)
y <- c(1, 2267, 7914, 14540, 21261, 28700, 36647, 43381, 49306, 54509, 
58749, 61197, 62578, 63394, 63820, 63975, 64040, 64058, 64058)
xout = 10:80
data_in <- tibble(x = x, y = y)
data_out <- 
interpolate(data_in, method = "pchip", xout = 10:80)
check_interpolate(data_in, data_out)

}
