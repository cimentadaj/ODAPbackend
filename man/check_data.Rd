% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkers.R
\name{check_data}
\alias{check_data}
\title{\code{check_data}}
\usage{
check_data(data)
}
\arguments{
\item{data}{tibble. A tibble generated by the output of the read_data function.}
}
\description{
Upper level function that checks the data quality. Checks if the columns are numeric, if any of the columns is missing, if there is insufficient rows, if there are missing data entries, if ages do not start with 0, and also if ages are coherent, sequential and not redundant.
}
\examples{
library(tibble)
Exposures <- c(100958,466275,624134,559559,446736,370653,301862,249409,
              247473,223014,172260,149338,127242,105715,79614,53660,
              31021,16805,8000,4000,2000,1000)

Deaths <- c(8674,1592,618,411,755,1098,1100,1357,
           1335,3257,2200,4023,2167,4578,2956,4212,
           2887,2351,1500,900,500,300)

data <- tibble(Deaths = Deaths,
                       Exposures = Exposures,
                       Age = c(0, 1, seq(5, 100, by = 5)),
                       AgeInt = c(diff(Age), NA))

check_data(
    data = data)
    
}
