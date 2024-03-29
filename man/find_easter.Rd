% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{find_easter}
\alias{find_easter}
\title{Find Easter date by year}
\source{
\url{https://en.wikipedia.org/wiki/Date_of_Easter#Algorithms}
}
\usage{
find_easter(year)
}
\arguments{
\item{year}{character or numeric for 4 digit year.}
}
\value{
Date object in YYYY-MM-DD format.
}
\description{
Determine the day of the year Easter occurs. Coded using the Gauss algorithm.
}
\examples{
find_easter(2021)
lapply(c(1990:2000), find_easter)
}
