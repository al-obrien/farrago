% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{set_renv_shared_cache}
\alias{set_renv_shared_cache}
\title{Set .Renviron to point to global RENV cache}
\usage{
set_renv_shared_cache(
  cache_loc,
  work_loc = getwd(),
  env_var = "RENV_PATHS_CACHE",
  prompt = TRUE
)
}
\arguments{
\item{cache_loc}{Location of a (ideally global) cache location for {renv}.}

\item{work_loc}{Working environment in which to search, create/append the contents (default: \code{getwd()}).}

\item{env_var}{The key name in the .Renviron file, as determined in {renv}, (default \code{RENV_PATHS_CACHE}).}

\item{prompt}{Boolean, to determine if process checking occurs or to accept overwriting defaults.}
}
\description{
Helper function to create or append the global library cache location for {renv}. Several checks will be performed, which, if \code{prompt} is
set to \code{TRUE} will determine the behavior. First, a search will be performed for an existing .Renviron file. Second, if the environment variable
named 'RENV_PATHS_CACHE' is set, it will ask if any existing key-value pair by this name in an .Renviron file should be removed prior to writing a new entry.
This function can only be used in interactive mode.
}
\examples{
loc <- 'loc/to/my/global/cache' # Should be known in advance for the team sharing the cache...
set_renv_shared_cache(cache_loc = loc)
set_renv_shared_cache(loc, prompt = FALSE)
}
