% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transfer.R
\name{load_csv}
\alias{load_csv}
\title{Load multiple CSV files from file directories}
\usage{
load_csv(path, pattern, ...)
}
\arguments{
\item{path}{A character string for the file path where csv files are located.}

\item{pattern}{A character vector passed to \code{list.files} to determine which files to load from the path.}

\item{...}{Additional parameters passed to \code{read_csv}.}
}
\value{
Datasets for all csv files that match desired pattern.
}
\description{
\code{load_csv} will load take an input path which contains multiple csv files, based upon the pattern provided,
all target csv files will be loaded into the global environment.
}
