% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.R
\name{convert_num2factor}
\alias{convert_num2factor}
\title{Convert numbers to factors}
\usage{
convert_num2factor(data)
}
\arguments{
\item{data}{Data column}
}
\value{
Dataset with reformatted columns.
}
\description{
\code{convert_num2factor} converts a column of numbers to a factor format, which is sometimes useful for working with ages.
}
\examples{
convert_num2factor(iris$Sepal.Width)

}
