#' Convert column formats
#'
#' \code{convert_format} will accept a character vector of column names and convert them to a desired format using functions
#' such as \code{\link{as.factor}}.
#'
#' @param data Data object.
#' @param func Function for converting data types.
#' @param col_list Character vector of column names.
#' @param ... Additional parameters to pass to the function provided.
#' @return Dataset with reformatted columns.
#'
#' @examples
#' convert_format(iris, factor, c('Sepal.Width'))
#'
#' @export
convert_format <- function(data, func, col_list, ...) {
  col_index <- which(attributes(data)$`names` %in% col_list)
  for (i in 1:length(unique(col_index))) {
    data[[col_index[i]]] <- func(data[[col_index[i]]], ...)
  }
  return(data)
}

#' Convert numbers to factors
#'
#' \code{convert_num2factor} converts a column of numbers to a factor format, which is sometimes useful for working with ages.
#'
#' @param data Data column
#' @return Dataset with reformatted columns.
#'
#' @examples
#' convert_num2factor(iris$Sepal.Width)
#'
#' @export
convert_num2factor <- function(data) {
  return(forcats::as_factor(stringr::str_trim(as.character(data))))
}

#' Convert number vector to percents
#'
#' \code{convert_num2percent} will convert all numbers into percent values with a defined precision.
#' As an alternative, \code{\link[scales]{percent}} or \code{\link[scales]{label_percent}} can be used.
#'
#' @param x Integer vector.
#' @param digits Integer for rounding precision.
#' @param format Character vector ('f' or 'g') passed to \code{\link{formatC}}.
#' @param suffix Character value to append to the end of the number (default is '\%').
#' @param ... Additional parameters passed to \code{\link{formatC}}.
#' @return Character vector.
#'
#' @examples
#' x <- c(-1, 0, 0.1, 0.5667, 1, 100.2)
#' convert_num2percent(x)
#'
#' @author Richie Cotton.
#' @source Implemented from \href{https://stackoverflow.com/users/134830/richie-cotton}{Richie Cotton's} StackOverflow contribution:
#' \url{https://stackoverflow.com/questions/7145826/how-to-format-a-number-as-percentage-in-r}
#' @seealso \code{\link[scales]{percent}}
#' \code{\link[scales]{label_percent}} (e.g. \code{label_percent()(x)})
#' @export
convert_num2percent <- function(x, digits = 2, format = "f", suffix = "%", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), suffix)
}


#' Convert all columns with blank data to NA
#'
#' \code{convert_blank2NA} converts a column of with blank ("") to \code{NA}. Be certain to check column data types afterwards,
#' as dates and factors may be affected. Alternative: use related \code{tidyverse} verbs that can achieve a similar effect in
#' perhaps a few more steps. To improve the speed of conversion between dates and characters, a default format is provided. For large
#' data-sets, the data copy can be substantial and modifying the data in-place may be prefereable (uses \code{data.table} package)
#'
#' @param data Data object.
#' @param target Replace all "" by default. Can replace with vector (e.g. \code{c('', ' ')})
#' @param skip_dates Logical; exclude date columns from operation.
#' @param modify_inplace Logical; uses data.table formats to reduce copies.
#' @return Dataset with NA instead of blank ("") cells.
#'
#' @examples
#' blankData <- data.frame(x = c(1, "", 2), y = c("", "Cool Stuff", "More Stuff"))
#' convert_blank2NA(blankData)
#'
#' @export
convert_blank2NA <- function(data, target = "", skip_dates = FALSE, modify_inplace = FALSE) {
  if(!is.logical(modify_inplace) || !is.logical(skip_dates)) stop('modify_inplace and skip_dates need to be logical values (TRUE or FALSE only).')
  if(skip_dates) warning('Skipping columns that inherit from Date, they will not be altered.')
  if(data.table::is.data.table(data)) stop('Please provide as tibble or data.frame only.')

  if (tibble::is_tibble(data)) {
    markConverted <- TRUE # Keep track if I converted the data
    data <- as.data.frame(data) # I need this to be a data.frame so that I can have it return a vector!
  } else {
    markConverted <- FALSE
  }

  # Detect date cols
  if(skip_dates) {
    date_cols_skip <- which(purrr::map_lgl(data, ~inherits(., 'Date')))
    date_cols_keep <- which(purrr::map_lgl(data, ~!inherits(., 'Date')))
  }

  # Select main method
  switch(as.character(modify_inplace),
         'TRUE' = {
           data <- data.table::as.data.table(data)

           if(skip_dates){
             for (j in date_cols_keep) {
               data.table::set(data,
                               i = which(data.table::`%chin%`(as.character(data[[j]]), target)),
                               j = j,
                               value = NA)
             }
           } else {
             for (j in seq_len(ncol(data))) {
               data.table::set(data,
                               i = which(if(inherits(data[[j]], 'Date')) { data.table::`%chin%`(as.character(data[[j]], format = "%Y-%m-%d"),target)
                               } else { data.table::`%chin%`(as.character(data[[j]]), target)}),
                               j = j,
                               value = NA)}
           }
         },

         'FALSE' = {
           # Define column changes to NA
           col2NA <- function(coldata, target = target, date = FALSE) {
             # temp <- gsub("\\s+", "", coldata) # replaces spaces with "", which I may not want
             if(date){
               coldata[as.character(coldata, format  = '%Y-%m-%d') %in% target] <- NA
             } else {
               coldata[as.character(coldata) %in% target] <- NA # Convert it to character temporarily for those that are Dates or otherwise throw errors!!!
             }
             return(coldata)
           }

           if(skip_dates) {
             for (i in date_cols_keep) {data[, i] <- col2NA(data[, i], target)}
           } else {
             for (i in 1:ncol(data)) {if(inherits(data[[i]], 'Date')) {data[, i] <- col2NA(data[, i], target, TRUE)
             } else {data[, i] <- col2NA(data[, i], target)}
             }
           }
         }
  )

  # Return values based on type
  if (markConverted) {
    return(tibble::as_tibble(data)) # Convert back to a tibble...
  } else {
    return(as.data.frame(data)) # In case it was modified as a data.table
  }
}


#' Convert elements to a new value
#'
#' \code{convert_elements} will convert a set of values in a column to a designated replacement value. This can be
#' viewed as a shortcut to using \code{\link[forcats]{fct_collapse}}. Can also accept 'NA' as a target and should return
#' the input class for that column.
#'
#' @param data Data object.
#' @param col Character vector of column to conduct conversion.
#' @param target Character vector of levels for conversion.
#' @param replacement Value of replacement (typically a Character).
#' @param convert2Factor Logical vector, if \code{TRUE}, will convert characters to factors.
#' @return Dataset with reformatted columns.
#'
#' @export
convert_elements <- function(data, col, target, replacement = NA, convert2factor = T) {

  class_type <- class(data[[col]])

  if(!class_type %in% c('factor', 'integer', 'numeric', 'character', 'double')) stop('Unsupported class provided, only factor, integer, numeric, character, and double are accepted.')
  if(is.na(target)) {
    index <- which(is.na(data[[col]]))
  } else {
    data[, col] <- as.character(data[[col]]) # Convert to char
    index <- which(data[[col]] %in% target) # Replace all unknown/others with NA (missing is already NA by combinedData code)
  }
  data[index, col] <- replacement # Replace

  if(convert2factor == T) {
    data[, col] <- factor(data[[col]])
  } # Convert to factor and drop the unused levels

  # Ensure return value is the one provided (may not be entirely necessary)
  if(class_type == 'double' & !convert2factor) {
    data[, col] <- as.double(data[[col]])
    return(data)
  } else if (class_type == 'integer' & !convert2factor) {
    data[, col] <- as.integer(data[[col]])
    return(data)
  } else if (class_type == 'numeric' & !convert2factor) {
    data[, col] <- as.numeric(data[[col]])
    return(data)
  } else {
    return(data)
  }
}

#' Convert calendar week to flu week
#'
#' \code{convert_wk_calendar2flu} will convert calendar week to seasonal flu week.
#'
#' Converts between calendar weeks and seasonal flu weeks, which is often used in influenza reporting.
#' Calendar weeks starts from week 1 in January to week 52/53. Seasonal flu weeks begins at the start of the
#' influenza season (Week 1) up to the week prior to the following influenza season (week 52/53). The start point of
#' the influenza season can change from year to year. You can perform the opposite operation via \code{\link{convert_wk_flu2calendar}}.
#'
#' @param week An integer between 1 and 52 representing the calender week to convert.
#' @param flu_wk_start Week of the year that flu season begins; default set to 34.
#' @return An integer vector representing the flu week.
#'
#' @examples
#' convert_wk_calendar2flu(34)
#' convert_wk_calendar2flu(35)
#' convert_wk_calendar2flu(c(35, 32, 1, 23))
#' convert_wk_calendar2flu(35, 45)
#'
#' @seealso \code{\link{convert_wk_flu2calendar}}
#' @note Adapted from original with courtesy of M. Parsons.
#'
#' @export
convert_wk_calendar2flu <- function(week, flu_wk_start = 34) {

  # Data checks and conversions
  if(length(flu_wk_start)>1) stop("Parameter 'flu_wk_start' should only have a single value")
  if(!is.integer(week)) {week <- as.integer(as.character(week))}
  if(!is.integer(flu_wk_start)) {flu_wk_start <- as.integer(as.character(flu_wk_start))}
  if(!all(week %in% seq(1,52), flu_wk_start %in% seq(1,52))) {stop("Invalid input week. Please input between 1 and 52.")}

  diff <- 52 - flu_wk_start
  week <- ifelse(week > flu_wk_start,
                 ifelse(week < 53, week - flu_wk_start,
                        diff),
                 week + diff)

  return(week)
}

#' Convert flu week to calendar week
#'
#' \code{convert_wk_flu2calendar} will convert seasonal flu weeks to calendar weeks.
#'
#' Converts between seasonal flu weeks and calendar weeks, which is often used in influenza reporting.
#' Calendar weeks starts from week 1 in January to week 52/53. Seasonal flu weeks begins at the start of the
#' influenza season (Week 1) up to the week prior to the following influenza season (week 52/53). The start point of
#' the influenza season can change from year to year. You can perform the opposite operation via \code{\link{convert_wk_calendar2flu}}.
#'
#' @param week An integer between 1 and 52 representing the flu week to convert.
#' @param flu_wk_start Week of the year that flu season begins; default set to 34.
#' @return An integer vector representing the calendar week.
#'
#' @examples
#' convert_wk_flu2calendar(18)
#' convert_wk_flu2calendar(19)
#' convert_wk_flu2calendar(c(35, 32, 1, 23))
#' convert_wk_flu2calendar(18, 45)
#'
#' @seealso \code{\link{convert_wk_calendar2flu}}
#' @note Adapted from original with courtesy of M. Parsons.
#'
#' @export
convert_wk_flu2calendar <- function(week, flu_wk_start = 34) {

  # Data checks and conversions
  if(length(flu_wk_start)>1) stop("Parameter 'flu_wk_start' should only have a single value")
  if(is.character(week)) {week <- as.integer(as.character(week))}
  if(is.character(flu_wk_start)) {flu_wk_start <- as.integer(as.character(flu_wk_start))}
  if(!all(week %in% seq(1,52), flu_wk_start %in% seq(1,52))) {stop("Invalid input week. Please input between 1 and 52.")}

  diff <- 52 - flu_wk_start
  week <- ifelse(week > diff, week - diff, week + flu_wk_start)

  return(week)
}

#' Convert calendar date to flu season
#'
#' Convert a vector of input dates to flu season details. Output on week, month, year, and season are output by default.
#'
#' The primary purpose of this function is to reassign week 53 dates into week 1 and week 52. It will also provide flu season
#' assignment based upon a flu week start date as reference. To improve speed, indexing methods are used to assign values instead of
#' \code{\link{ifelse}} (increased speed by over 5-fold).
#'
#' @param date Character vector in date format of \code{'YYYY-mm-dd'}.
#' @param format Character vector following \code{\link[base]{strptime}}; defaults to \code{"\%Y-\%m-\%d"}.
#' @param flu_wk_start Week of the year that flu season begins, all entries prior to that week will be in prior season; default set to 35.
#' @param return_values Character vector of which values to return, default is set to all ('week', 'month', 'year', 'season').
#' @param split_wk53 Boolean value to determine if week 53 values are split based upon isoweek (Sunday start).
#' @param sunday_start Boolean value to determine if the start of a week is a Sunday. If set to \code{FALSE}, Monday is the start of the week.
#' @return List containing vectors of week, month, year, and season related to provided dates.
#'
#' @seealso \code{\link{convert_wk_flu2calendar}}, \code{\link{convert_wk_calendar2flu}}
#' @note Adapted from original with courtesy of M. Ware.
#' @examples
#'
#' # Basic examples
#' date_list <- c('2022-01-01', '2021-01-01', '2020-08-30', '2020-09-01', '2020-09-23', '2020-01-01', '2019-12-31', '2018-01-01', '2017-01-01', '2016-01-01')
#' convert_date2fluseason(date_list)
#' convert_date2fluseason(date_list, return_values = 'season')
#' convert_date2fluseason(date_list, flu_wk_start = 40)
#'
#' # Detailed example
#' library(dplyr)
#' library(tidyr)
#'
#' # Create test data for known cases by season
#' test_data <- data.frame(season = c(1, 1, 1, 2, 2),
#'                         date = lubridate::ymd(c('2020-06-14', '2020-08-09', '2020-08-16', '2021-08-29', '2021-09-12', '2021-01-01')),
#'                         n = c(1,1,1,1, 2))
#'
#' # Determine the season with the specific date2fluseason function
#' test_data$wk <- AHRtools::convert_date2fluseason(test_data$date, return_values = 'week')$week
#' test_data$yr <- AHRtools::convert_date2fluseason(test_data$date, return_values = 'year')$year
#'
#' # To fill empty periods join to full combination of season, year, and week
#' test_data <- full_join(test_data, expand(test_data, season, yr, wk = 1:52), by = c('season', 'yr', 'wk'))
#' test_data$n <- ifelse(is.na(test_data$n), 0 , test_data$n)
#' test_data <-  arrange(test_data, season, wk)
#' test_data$date <- if_else(is.na(test_data$date),
#'                           ISOweek::ISOweek2date(paste(test_data$yr, paste0('W',test_data$wk), 1, sep = '-')),
#'                           test_data$date)
#'
#' @export
convert_date2fluseason <- function(date, format = '%Y-%m-%d', flu_wk_start = 35, return_values, split_wk53 = TRUE, sunday_start = TRUE){

  valid_returns <- c('week', 'month', 'year', 'season')
  if(!missing(return_values)) match.arg(return_values, valid_returns)

  # Run through parsing check for YYYY-MM-DD formats
  date <- lubridate::as_date(date, format = format)

  # Parse year, month, wk number, wk day (vectors)
  year_test <- lubridate::year(date)
  month <- lubridate::month(date)
  if(sunday_start) {fmt_wk <- lubridate::isoweek(date + 1)} else {fmt_wk <- lubridate::isoweek(date)} # +1 for Sunday start...
  week_day <- lubridate::wday(date)

  if(split_wk53){
    # Calculate conditions for weeks and season
    fmt_wk[fmt_wk == 53 & month == 12] <- 52 # Roll back to week 52 if in prior year, Dec
    fmt_wk[fmt_wk == 53 & month == 1] <- 1 # Roll into week 1 if in next year, Jan

    # week <- fmt_wk
    # index1 <- week_day == 1 & fmt_wk + 1 == 53 & month == 12 # if Sunday, in Dec & just shy of week 53 (will force to 52)
    # index2 <- !index1 & week_day == 1 # Sunday but not matched to prior index
    # week[index1] <- 52
    # week[index2] <- fmt_wk[index2] + 1
  }
  week <- fmt_wk


  year <- year_test
  index3 <- week==1 & month == 12
  year[index3] <- year_test[index3]+1

  season <- rep(NA, length(fmt_wk))
  index4 <- fmt_wk < flu_wk_start | (fmt_wk == 52 & month == 1) # Go back a year for season for those in 52 wk but next year month
  index5 <- !index4 & fmt_wk == 53 & month == 12
  season[index4] <- paste(year[index4]-1, year[index4], sep="-")
  season[index5 & is.na(season)] <- paste(year[index5 & is.na(season)]+1, year[index5 & is.na(season)]+2, sep="-")
  season[!index5 & is.na(season)] <-  paste(year[!index5 & is.na(season)],year[!index5 & is.na(season)]+1, sep="-")

  out <- setNames(list(week, month, year, season),
                  valid_returns)

  if(!missing(return_values)) {
    return(out[return_values])
  } else {
    return(out)
  }
}


#' Convert a table object to a png file
#'
#' \code{convert_table2png} will take various types of table object formats and convert them to a png image.
#'
#' To create the png file, a LaTeX distribution, and texi2dvi (from base R) are required. It is recommended to use
#' \code{\link[tinytex]{install_tinytex}}. Various LaTeX packages may be needed, specifically 'tabularx' and 'dvipng' should be
#' installed using \code{\link[tinytex]{tlmgr_install('dvipng')}}
#'
#' @param obj A table object (huxtable, xtable, or tables).
#' @param name Character vector for naming output.
#' @param rez Integer value for desired resolution (default is 600).
#' @param interpreter Character vector to define which type of table object was used (huxtable, xtable, or tables).
#' @return A png file saved to working directory.
#'
#' @examples
#' \dontrun{
#' convert_table2png(mtcars, 'carsTable', 600, interpreter = 'xtable')
#' }
#'
#' @source Inspired/adapted by a post from Michael Yan's blog: \url{http://thinkdatascience.com/}
#'
#' @export
convert_table2png <- function(obj, name, rez = 600, interpreter = "xtable") { # Input the table object, the file name, and resolution of output
  message("This function requires various latex packages to be installed. If it failed, try loading tinytex in R or install MikTex")
  first <- name
  name <- paste(name, ".tex", sep = "") # Will be a tex file
  sink(file = name) # Places file in the working directory
  cat("\n      \\documentclass{report}\n      \\usepackage[paperwidth=5.5in,paperheight=7in,noheadfoot,margin=0in]{geometry}\n      \\usepackage{array}\n      \\usepackage{caption}\n      \\usepackage{graphicx}\n      \\usepackage{siunitx}\n   \\usepackage{booktabs}\n   \\usepackage[table]{xcolor}\n      \\usepackage{multirow}\n      \\usepackage{hhline}\n      \\usepackage{calc}\n      \\usepackage{tabularx}\n \\usepackage{threeparttable}\n   \\usepackage{longtable}\n \\usepackage{threeparttablex}\n   \\begin{document}\\pagestyle{empty}\n      ") # Header for the latex code
  if (interpreter == "xtable") {
    tryCatch({print(xtable::xtable(obj))},
             error = function(e){
               print(e)
               return(NA)})# Converts to xtable object (LATEX code), if there is a bad error, the tryCatch will print the error (usually not the right table format) to the image (otherwise all of R will freeze)
  } else if (interpreter == "huxtable") {
    huxtable::print_latex(huxtable::as_hux(obj))
  } else if(interpreter == "tables") {
    Hmisc::latex(obj)
  }
  cat("\n      \\end{document}\n      ")
  sink() # Adds the above code to initially sinked file
  tools::texi2dvi(file = name)
  cmd <- paste("dvipng -T tight", paste(" -D ", rez), shQuote(paste(first, ".dvi", sep = ""))) # Use option -D to adjust resolution
  invisible(system(cmd))

  # If you want to add a parameter to have PDF output as well try either of these:
  #if(output == 'pdf'){

  # METHOD 1
  #tools::texi2dvi(file = name, pdf = T)

  # METHOD 2, read in the PNG just made, make it a grob, then draw it into a PDF file
  # rasterGrob(readPNG('')) -> temp1
  # pdf()
  # grid.draw()
  # dev.off()
  #}

  cleaner <- c(".tex", ".aux", ".log", ".dvi")
  invisible(file.remove(paste(first, cleaner, sep = "")))
}

#' Convert a table to wide format
#'
#' \code{convert_2wide} will convert a dataframe with multiple observations per group into one row with multiple columns.
#'
#' The primary use case for this functions is to combine an individual with multiple time points (e.g. visits to clinic) into a single row with one column per
#' time point of interest. It is recommended to arrange (\code{\link[dplyr]{arrange}}) columns of interest first, this ensures the first instance is actually the date before the next in sequence.
#' Re-ordering the columns may not work as expected unless the function is adjusted so that numeric value comes first, if that is the case, the columns could be arranged
#' by \code{select(.,rcpt_uli, contains("1"), contains("2"), everything())}.
#'
#' @param data A data object.
#' @param group_key Name of grouping column, typicaly a unique ID.
#' @param value Name of columns to be passed to \code{value} in \code{\link[tidyr]{gather}}.
#' @param order_num Character vector to define which type of table object was used (huxtable, xtable, or tables).
#' @param fix_dates Logical vector, if \code{TRUE} date formats will be corrected.
#' @param date_identifier Character vector to identify date columns.
#' @return A converted data object in wide format.
#'
#' @examples
#' \dontrun{
#' date_data <- data.frame(ID = c(123, 124, 125), AdmitDate = as.Date(c("2014-04-05", NA, "2016-02-03")), DOB = as.Date(c("1990-01-01", NA, NA)))
#' date_data <- dplyr::arrange(date_data, AdmitDate, DOB)
#' WideFormat <- convert_2wide(date_data, ID, value = AdmitDate:DOB)
#'
#' # Alternative method is using tidyr::pivot_wider after naming groups
#' date_data <- data.frame(ID = c(123, 124, 125), AdmitDate = as.Date(c("2014-04-05", NA, "2016-02-03")), DOB = as.Date(c("1990-01-01", NA, NA)))
#' WideFormat <- dplyr::arrange(date_data, AdmitDate, DOB) %>%
#'    dplyr::group_by(ID) %>%
#'    dplyr::mutate(track = dplyr::row_number()) %>% dplyr::ungroup %>%
#'    tidyr::pivot_wider(names_from = track, values_from = c(AdmitDate, DOB)
#'}
#'
#' @source Adapted from \href{https://stackoverflow.com/users/3732271/akrun}{Akrun's} StackOverflow post:
#' \url{https://stackoverflow.com/questions/43695424/tidyr-spread-multiple-columns}
#'
#' @seealso To improve the function by doing spread operations separately and then joining back together, the following reference can be used:
#' \url{https://stats.idre.ucla.edu/sas/modules/how-to-reshape-data-long-to-wide-using-proc-transpose/}. Refer to \code{\link[tidyr]{pivot_wider}} for a similar approach.
#'
#' @importFrom magrittr %>%
#'
#' @note \code{gather} is a superseded function from \code{tidyr} and may eventually be completely replaced by alternatives like \code{pivot_wider}.
#'
#' @export
convert_2wide <- function(data, group_key, value, order_num = F, fix_dates = T, date_identifier = "DATE"){
  # Quote
  keyq <- rlang::enquos(group_key)

  valueq <- rlang::enquos(value)

  # If statement that doesnt evaluate both arguments and isn't vectorized like `ifelse`
  iff <- function(cond,x,y) {
    if(cond) return(x) else return(y)
  }

  # Operation
  data %>%
    tidyr::gather(Var, Val, !!!valueq) %>%
    dplyr::group_by(!!!keyq, Var) %>%
    dplyr::mutate(n = dplyr::row_number()) %>%
    {if(order_num == T) tidyr::unite(., VarTemp, Var, n, sep = "") else tidyr::unite(., VarTemp, n, Var, sep = "_")} %>%
    tidyr::spread(VarTemp, Val) %>%
    iff(fix_dates, dplyr::mutate_at(., dplyr::vars(contains(date_identifier)), .funs = function(x) lubridate::as_date(as.numeric(x))), .) %>%
    readr::type_convert()
}
