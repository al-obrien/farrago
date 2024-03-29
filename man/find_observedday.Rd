% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{find_observedday}
\alias{find_observedday}
\title{Find observed holiday over weekends}
\usage{
find_observedday(year, month, day)
}
\arguments{
\item{year}{character or numeric for 4 digit year.}

\item{month}{character or numeric (01-12) for month of which holiday of interest occurs.}

\item{day}{character or numeric (01-31) for day of which holiday of interest occurs.}
}
\value{
Date object in YYYY-MM-DD format.
}
\description{
When a holiday lands on Saturday or Sunday, they are typically observed the following Monday. Boxing day is an exception and will
occur on Tuesday if Dec-26 occurs on a Monday.
}
\examples{
find_observedday(2021, month = 12, day = 25)
lapply(c(1990:2000), find_observedday, month = 12, day = 25)
}
