% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate.R
\name{calculate_cruderate}
\alias{calculate_cruderate}
\title{Calculate crude rates}
\usage{
calculate_cruderate(data, numerator, denominator, per = 1e+05)
}
\arguments{
\item{data}{A data object.}

\item{numerator}{Character vector of the column name.}

\item{denominator}{Character vector of the column name.}

\item{per}{Numeric value for rate scale (i.e. per X population).}
}
\value{
A new dataset (\code{tibble}) with crude rate and standard error added.
}
\description{
\code{calculate_cruderate} performs calculations for a crude rate per 100,000 (by default) and associated standard errors. If the numerator is 0, the
standard error is determined by the modified Wald interval. Missing numerator values will be filled with zero. When denominator is NA,
the rates/SE will not occur.
}
\examples{
tempData <- data.frame(cases = as.numeric(c(1, 0 , 100, 10, NA)), population = as.numeric(c(10, 1000, 3200, NA, 1000)))
calculate_cruderate(tempData, 'cases', 'population')

}
