#' Count decimal places
#'
#' Examine an number and return the number of decimals present.
#'
#' Uses conversions from a numeric to character to splice off and count how many decimals are provided.
#' Supports vectorization.
#'
#' @param x a vector of numeric values.
#' @return Length of each values decimal places.
#' @export
#' @examples
#' testing <- c(0, 100, 100.1, 101.10101, 100.00)
#' count_decimals(testing)
count_decimals <- function(x) {
  stopifnot(class(x)=="numeric")

  # Only calculate on those the need to change from 0
  index1 <- which((x %% 1) != 0)
  index2 <- which((x %% 1) == 0)

  # Need to push it through 1 at a time so that it doesnt add zeros
  x[index1] <- vapply(x[index1],
                      function(x) {
                        str <- as.character(x)
                        out <- nchar(strsplit(str, "\\.")[[1]][2])
                        if(is.na(out)) {
                          warning('NA converted to 0 in decimal count');
                          0;
                        } else {
                          out
                        }
                      }, numeric(1))

  x[index2] <- 0

  return(x)
}

#' Create string vector for loading SAS libraries
#'
#' \code{connect_sas_libraries} will create a character string of SAS code for loading the provided library and path information.
#' This is typically passed as an argument to \code{add_library} to \code{\link{load_from_sas}}. Assumes connecting to \emph{remote} libraries.
#'
#' @param lib_name A character vector for the SAS library name to be defined.
#' @param lib_path A character variable for the connection path to the library of interest.
#' @return A character vector of SAS code for loading desired libraries.
#' @examples
#' \dontrun{
#' connect_sas_libraries(lib_name = 'mylib', lib_path = '/location/to/saslibrary/on/drive')
#' }
#' @export
connect_sas_libraries <- function(lib_name = "temp", lib_path = "") {
  outputstring <- paste0(
    "RSUBMIT;", "\n",
    "libname ", lib_name, " \"", lib_path, "\"", "; \n",
    "ENDRSUBMIT; \n",
    "libname ", lib_name, " server = SASAPP slibref = ", lib_name, "; \n"
  )
  return(outputstring)
}

#' Rename and relocate a file
#'
#' \code{rename_file} will rename and/or move a file; this is useful for adjusting R markdown output locations.
#'
#' @param from File path and name of original file.
#' @param to File path and name of altered/moved file.
#'
#' @export
rename_file <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}

#' Convert Windows-style paths to UNIX-style
#'
#' \code{repath} will convert a Windows-style file path to UNIX-style, which can then be used in R code. The function
#' is interactive and should be called without any arguments provided. A prompt will appear for input and the converted
#' file path will be copied to the clipboard.
#'
#' @source Adapted from \href{https://stackoverflow.com/users/378085/tom}{Tom's} stackoverflow contribution:
#' \url{https://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile/12703931#12703931}
#'
#' @examples
#' repath()
#'
#' @export
repath <- function() {
  cat("Paste windows file path and hit RETURN twice")
  x <- scan(what = "")
  xa <- gsub("\\\\", "/", x)
  writeClipboard(paste(xa, collapse = " "))
  cat("De-windowsified path provided; also on the clipboard.)\n", xa, "\n")
}

#' Reorder the levels within groups
#'
#' A set of functions to reorder factors within faceted groups, useful for plotting bar charts with \code{ggplot2}.
#'
#' First use \code{reorder_within} during \code{aes()} step for the factor level and the variable used for faceting.
#' Then, use the scale functions (\code{scale_xreordered}, \code{scale_y_reordered}) to adjust the scale to re-order.
#'
#' @param x Vector to reorder.
#' @param by Vector, of same length as \code{x}, used as reference for reordering.
#' @param within Vector, of same length as \code{x}, used for facetting.
#' @param fun Function used within subsets to determine the ordering result (\code{mean} by default).
#' @param sep Character used to separate labels for the reordering.
#' @param ... Additional arguments passed to \code{\link{reorder}} for \code{reorder_within}. For scale functions,
#' additional arguments passed to either \code{\link[ggplot2]{scale_y_discrete}} or \code{\link[ggplot2]{scale_x_discrete}}.
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(tidyr)
#'
#' iris_gathered <- gather(iris, metric, value, -Species)
#'
#' ggplot(iris_gathered, aes(reorder_within(Species, value, metric), value)) +
#'   geom_bar(stat = "identity") +
#'   scale_x_reordered() +
#'   facet_wrap(~ metric, scales = "free_x")
#' @author David Robinson.
#' @source David Robinson's personal package, specifically "reoder_within.R"
#' \url{https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R}
#'
#' @seealso "Ordering categories within gplot2 Facets" by Tyler Rinker:
#' \url{https://trinkerrstuff.wordpress.com/2016/12/23/ordering-categories-within-ggplot2-facets/}
#'
#' @export
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

#' @rdname reorder_within
#' @export
scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

#' @rdname reorder_within
#' @export
scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}

#' Create a random string of n length.
#'
#' Using \code{\link{paste0}} and \code{\link{sample}}, a specified length of randomly selected letters and numbers
#' will be pasted together.
#'
#' @param length Numeric value, will be coerced into an integer. Defaults to \code{8L}.
#' @return Character vector matching provided \code{length}.
#' @export
#'
#' @examples
#' create_randomstring()
#' create_randomstring(5L)
#' create_randomstring(17)
#'
create_randomstring <- function(length = 8L) {
  if(length > 100) stop('Random string length is over 100, use a smaller length.')
  if(!is.integer(length)) length <- as.integer(length)

  rnd_combo <- sample(c(LETTERS, letters, 0:9), replace = TRUE, size = length)

  paste0(rnd_combo, collapse = '')
}

#' Determine **all** duplicated elements
#'
#' Similar to \link{duplicated} but returns all elements that are identified as a
#' duplicate element (i.e. not just the duplicated elements following the first identified
#' row as is default in R base \code{duplicated}).
#'
#' Should also work with \code{data.table} objects. To index all duplicated elements by multiple fields,
#' provide them as a vector of names to the dataset. \code{data.table} uses \code{by} as a parameter for
#' a similar purpose but this function follows the base R syntax of \code{duplicated()}.
#'
#' @param x A vector of values to determine duplicated elements (can be 1 or more columns).
#' @param ... Additional parameters passed to \code{\link{duplicated}}
#' @return Logical vector of index for duplicated elements.
#' @export
#' @examples
#' testing <- data.frame(col1 = c(1, 1, 1, 2, 3, 3, 3),
#'                       col2 = c(0, 100, 0, 100, 100.1, 101.10101, 100.00))
#' index1 <- duplicated_all(testing[,c('col1')])
#' index2 <- duplicated_all(testing[,c('col2')])
#' index3 <- duplicated_all(testing[,c('col1', 'col2')])
#' testing[index1,]
#' testing[index2,]
#' testing[index3,]
duplicated_all <- function(x, ...) {
  duplicated(x, ...)|duplicated(x, fromLast = TRUE, ...)
}

#' Find files containing pattern match
#'
#' Identify files within provided path that contain text that match a particular pattern.
#'
#' This function is often useful to quickly scan folders containing code that you need to find a particular key-phrase.
#'
#' @param path File path that contains files of interest (can look into child folders using \code{recursive = T})
#' @param file_pattern Basic regular expression to pick up file name pattern (e.g. \code{'\\.(R|Rmd)'})
#' @param text_pattern Grab the n-th file from the list (descending order by date). Default set to \code{NULL}, entire list is returned. Set to \code{1} if want just the latest match returned.
#' @param recursive Reduce set of discovered file based upon a date threshold (default: \code{NULL}, no filtering occurs).
#' @param locale Passed to \code{\link[readr]{read_file}}, controls encoding and similar file properties.
#' @param ... Additional parameters passed to \code{\link{grepl}}, helpful to add \code{perl} capabilities in pattern search.
#'
#' @return Returns character vector for file names with matches
#' @export
#' @examples
#'
#' # Find pattern within set of files
#' find_file_content('./R', file_pattern = '\\.(R|Rmd)', text_pattern = 'sapply')
#'
find_file_content <- function(path, file_pattern = NULL, text_pattern, recursive = FALSE, locale = readr::default_locale(), ...) {

  file_list <- list.files(path, recursive = recursive, pattern = file_pattern, full.names = T)
  file_names <- basename(file_list)

  file_names[sapply(X = as.list(file_list), FUN = function(x) grepl(pattern = text_pattern, readr::read_file(x, locale = locale), ...))]
}

#' Find the all dates for weekday in a month
#'
#' Determine the exact days (i.e. YYYY-MM-DD) that a specific day of the week falls on.
#'
#' @param year character or numeric for 4 digit year.
#' @param month character or numeric (01-12) for month.
#' @param weekday character value for day of the week (e.g. 'Monday').
#' @return Date vector object in YYYY-MM-DD format.
#' @export
#' @examples
#' list_weekdays(2021, 12, 'Monday')
#' list_weekdays(2021, 12, 'Monday')[1]
#' list_weekdays('2021', 12, 'Monday')
#' lapply(c(1990:2000), list_weekdays, month = 12, weekday = 'Monday')
list_weekdays <- function(year, month, weekday) {
  match.arg(weekday, choices = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))

  year_month <- paste(year, month, sep = '-')
  date_min <- lubridate::ymd(paste(year_month, '01', sep = '-'))
  date_max <- lubridate::ceiling_date(date_min, unit = 'month')-1
  day_range <- seq(date_min, date_max, by = 'day')

  day_range[grepl(weekday, weekdays(day_range))]
}

#' Find Easter date by year
#'
#' Determine the day of the year Easter occurs. Coded using the Gauss algorithm.
#'
#' @source \url{https://en.wikipedia.org/wiki/Date_of_Easter#Algorithms}
#' @param year character or numeric for 4 digit year.
#' @return Date object in YYYY-MM-DD format.
#' @export
#' @examples
#' find_easter(2021)
#' lapply(c(1990:2000), find_easter)
find_easter <- function(year){

  a <- year %% 19
  b <- year %% 4
  c <- year %% 7

  k <- year %/% 100
  p <- ((13 + 8*k) %/% 25)
  q <- k %/% 4
  M <- (15 - p + k - q) %% 30
  N <- (4 + k - q) %% 7

  d <- (19 * a + M) %% 30
  e <- (2*b + 4*c + 6*d + N) %% 7
  e_day <- 22 + d + e

  if(d == 29 && e == 6) {
    e_month <- 04; e_day = 19;
  } else if (d == 28 && e == 6 && (11 * M + 11) %% 30 < 19) {
    e_month <- 04; e_day = 18;
  } else {
    if (e_day > 31) {
      e_month <- 04; e_day <- e_day - 31
    } else {
      e_month <- 03
    }
  }
  return(lubridate::ymd(paste(year, e_month, e_day, sep = '-')))
}

#' Find Victoria day by year
#'
#' Determine the day of the year Victoria day (Monday before 25th of May) is observed as a holiday.
#'
#' @param year character or numeric for 4 digit year.
#' @return Date object in YYYY-MM-DD format.
#' @export
#' @examples
#' find_victoriaday(2021)
#' lapply(c(1990:2000), find_victoriaday)
find_victoriaday <- function(year){
  bday <- lubridate::ymd(paste(year, '05', '25', sep = '-'))
  bday_wd <- lubridate::wday(bday, week_start = 1)
  if(bday_wd == 1) return(bday - 7) else return(bday - bday_wd + 1)
}

#' Find observed holiday over weekends
#'
#' When a holiday lands on Saturday or Sunday, they are typically observed the following Monday. Boxing day is an exception and will
#' occur on Tuesday if Dec-26 occurs on a Monday.
#'
#' @param year character or numeric for 4 digit year.
#' @param month  character or numeric (01-12) for month of which holiday of interest occurs.
#' @param day character or numeric (01-31) for day of which holiday of interest occurs.
#' @return Date object in YYYY-MM-DD format.
#' @export
#' @examples
#' find_observedday(2021, month = 12, day = 25)
#' lapply(c(1990:2000), find_observedday, month = 12, day = 25)
find_observedday <- function(year, month, day){
  date_c <- lubridate::ymd(paste(year, month, day, sep = '-'))
  if(weekdays(date_c) == 'Saturday') {
    return(date_c+2)
  } else if(weekdays(date_c) == 'Sunday' && (lubridate::month(date_c) == 12 && lubridate::day(date_c) == 26))  { # Boxing day logic
    return(date_c + 2)
  } else if(weekdays(date_c) == 'Monday' && (lubridate::month(date_c) == 12 && lubridate::day(date_c) == 26))  { # Boxing day logic
    return(date_c + 1)
  } else if(weekdays(date_c) == 'Sunday') {
    return(date_c + 1)
  } else {
    return(date_c)
  }
}


#' Set .Renviron to point to global RENV cache
#'
#' Helper function to create or append the global library cache location for {renv}. Several checks will be performed, which, if \code{prompt} is
#' set to \code{TRUE} will determine the behavior. First, a search will be performed for an existing .Renviron file. Second, if the environment variable
#' named 'RENV_PATHS_CACHE' is set, it will ask if any existing key-value pair by this name in an .Renviron file should be removed prior to writing a new entry.
#' This function can only be used in interactive mode.
#'
#' @param cache_loc Location of a (ideally global) cache location for {renv}.
#' @param work_loc Working environment in which to search, create/append the contents (default: \code{getwd()}).
#' @param env_var The key name in the .Renviron file, as determined in {renv}, (default \code{RENV_PATHS_CACHE}).
#' @param prompt Boolean, to determine if process checking occurs or to accept overwriting defaults.
#' @export
#' @examples
#' loc <- 'loc/to/my/global/cache' # Should be known in advance for the team sharing the cache...
#' set_renv_shared_cache(cache_loc = loc)
#' set_renv_shared_cache(loc, prompt = FALSE)
set_renv_shared_cache <- function(cache_loc, work_loc = getwd(), env_var = 'RENV_PATHS_CACHE', prompt = TRUE) {

  if(!interactive()) { stop('Must use this function interactively...') }

  # Prompt to continue if exists (default is TRUE)
  check_r_environ <- function(work_loc, prompt) {
    if(!prompt) return(TRUE)

    msg <- paste('Continue [y/n]? ')

    if(file.exists(file.path(work_loc, '.Renviron'))) {
      cat('.Renviron found under:', work_loc, '\n')
      resp <- tolower(trimws(readline(msg)))
    } else {
      cat(paste0('.Renviron not found under: ', work_loc, ', will create one... \n'))
      resp <- tolower(trimws(readline(msg)))
    }
    return(substring(resp, 1L, 1L) == "y")
  }

  # Check for existing usage
  replace_renv_cache <- function(work_loc, env_var, prompt) {
    dir_loc <- Sys.getenv(env_var)
    if(dir_loc != '' && dir.exists(dir_loc) && file.exists(file.path(work_loc, '.Renviron'))) {

      # Determine if should prompt...
      if(prompt) {
        cat('Existing renv cache variable found...')
        resp <- tolower(trimws(readline('Replace [y/n]? ')))
      } else {
        resp <- 'y'
      }

      # If 'y' then remove prior rows...
      if(substring(resp, 1L, 1L) == "y") {
        tmp_path <- file.path(work_loc, '.Renviron')
        file_tmp <- readLines(tmp_path)
        indx <- grep(file_tmp, pattern = paste0(env_var, '\\b'))
        if(length(indx) > 0) writeLines(file_tmp[-indx], con = tmp_path)
        return(TRUE) # Exists, correct, continue
      } else {
        return(FALSE) # Exist and don't replace, exit
      }
    }

    return(TRUE) # Doesn't exist, continue

  }

  # Perform checks and write to file if pass
  if(check_r_environ(work_loc, prompt)) {
    renv_check <- replace_renv_cache(work_loc, env_var, prompt)
    if(renv_check) {

      # Write to new or existing...
      cat(paste0(env_var, ' = \"', cache_loc, "\"\n"),
          file = file.path(work_loc, '.Renviron'),
          append = TRUE)

      # Read renviron (perhaps Sys.setenv would be better?)
      readRenviron(file.path(work_loc, '.Renviron'))

    } else {
      warning('Function exiting without change to .Renviron for cache assignment...')
      return(invisible(NULL)) # Dont replace, break out
    }

  } else {
    warning('Function exiting without change to .Renviron for cache assignment...')
    return(invisible(NULL))
  }
}
