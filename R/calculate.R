#' Calculate trimesters and preterm births
#'
#' \code{calculate_trimesters} calculates dates for pregnancy trimesters and if the birth was pre-term.
#'
#' Uses the date of birth (required field) and any available information on the gestation time (in weeks)
#' to calculate the start and end of the trimesters, in separate columns, along with a flag for a suspected preterm birth.
#' If no gestation information is provided, the default average pregnancy length of 40 weeks is used. One can extend the length
#' of pregnancy by adjusting \code{avg_pregnancy} but it is advised to use \code{buffer} to extend the start date of the first trimester instead.
#' The average length of each trimester can also be adjusted from the default of 13 weeks; again, it is advised to leave this at the accepted default.
#'
#' @param data A data object.
#' @param dob The column name for date of birth.
#' @param gestation The column name containing the numeric weeks of gestation (optional).
#' @param avg_pregnancy Integer value for the expected number of weeks in an average pregnancy (default: 40).
#' @param avg_tri Integer value for the expected length of each trimester (default: 13 weeks).
#' @param buffer Integer value. Will extend the date of first trimester. Useful when you want to keep average pregnancy the same but want to include a higher maximum of pregnancies (i.e. longer third trimester).
#' @return A new dataset with the start and end dates of trimesters and whether or not the birth is suspected as pre-term.
#' @examples
#' tempData <- data.frame(date_of_birth = as.Date(c('1991-01-01', '1990-02-04', '1992-01-21')), gestation = c(35, 41, NA))
#' calculate_trimesters(tempData, date_of_birth, gestation)
#'
#' @importFrom  magrittr %>%
#' @export
calculate_trimesters <- function(data, dob, gestation, avg_pregnancy = 40, avg_tri = 13, buffer = 0) {
  # Stop checks
  if(rlang::quo_is_missing(rlang::enquo(dob))) stop('A variable for birth date must be provided')
  if(avg_pregnancy > 42 | avg_pregnancy < 37) stop('You have provided a non-sensical time for an average pregnancy, please input between 35 and 46 weeks. Try using buffer if you want to extend start point of unkown gestation.')
  if(buffer > 5 | buffer < 0) stop('Please only provide buffer values between 0 and 5')
  if(avg_tri > 15 | avg_tri < 13) stop('Average trimester should be between 13 and 15')

  # In case of no gestation variable create empty
  if(rlang::quo_is_missing(rlang::enquo(gestation))) {
    warning('No variable for gestation length was provided, all pregnancies will assume the average pregnancy length of: ',
            avg_pregnancy)

    gest_null <- TRUE
  } else {
    gest_null <- FALSE
  }

  # Set standard boundaries (counts back from birth date, so avg_pregnancy back from dob is the start of tri1)
  tri1wk_s = lubridate::dweeks(avg_pregnancy + buffer)
  tri1wk_e = tri1wk_s - lubridate::dweeks(avg_tri)
  tri2wk_s = tri1wk_e + lubridate::ddays(1)
  tri2wk_e = tri2wk_s - lubridate::dweeks(avg_tri)
  tri3wk_s = tri2wk_e + lubridate::ddays(1)

  # Create cut points (if gestation column exists, then see if the gestation exists, if not, then return TRUE and will also be taking default)
  if(gest_null) {
    data <- data %>%
      dplyr::mutate(tri1_s = {{dob}} - tri1wk_s,
             tri1_e = {{dob}} - tri1wk_e,
             tri2_s = {{dob}} - tri2wk_s,
             tri2_e = {{dob}} - tri2wk_e,
             tri3_s = {{dob}} - tri3wk_s)
  } else if (gest_null ==FALSE) {
    data <- data %>%
      dplyr::mutate(tri1_s = dplyr::if_else(is.na({{gestation}}), {{dob}} - tri1wk_s, {{dob}} - lubridate::dweeks({{gestation}})),
             tri1_e = dplyr::if_else(is.na({{gestation}}), {{dob}} - tri1wk_e, {{dob}} - lubridate::dweeks({{gestation}}) + lubridate::dweeks(avg_tri)),
             tri2_s = dplyr::if_else(is.na({{gestation}}), {{dob}} - tri2wk_s, {{dob}} - lubridate::dweeks({{gestation}}) + lubridate::dweeks(avg_tri) + lubridate::ddays(1)), # Add day to start in next week
             tri2_e = dplyr::if_else(is.na({{gestation}}), {{dob}} - tri2wk_e, {{dob}} - lubridate::dweeks({{gestation}}) + lubridate::dweeks(2*avg_tri)),
             tri3_s = dplyr::if_else(is.na({{gestation}}), {{dob}} - tri3wk_s, {{dob}} - lubridate::dweeks({{gestation}}) + lubridate::dweeks(2*avg_tri) + lubridate::ddays(1)))
  }

  # Clean up estimates for pre-term
  data <- data %>%
    dplyr::mutate_at(dplyr::vars(tri1_s, tri1_e, tri2_s, tri2_e, tri3_s),
                     function(x) dplyr::if_else(x > .[[rlang::as_name(rlang::enquo(dob))]], as.Date(NA), x)) %>%
    dplyr::mutate(preterm = dplyr::if_else(lubridate::days({{dob}} - tri1_s) <= lubridate::weeks(37), 1, 0)) # prior to 37 weeks is often considered preterm

  data
}

#' Collapse timesteps for cohorts
#'
#' \code{collapse_timesteps} will create a vector that flags rows in a cohort that have subsequent steps within the threshold difference (usually in days).
#'
#' Data when organized as a cohort will typically have a long-format with repeated records for an individual, each with a particular date-span for that instance.
#' Often, subsequent steps between these records are very close in time and can be collapse into a single record to simplify the cohort. The logic involves comparing
#' the previous records date_end compared to the subsequent date_start. If the difference in those two dates is more than a specific threshold, they will be flagged as
#' a different group in a progression of cohort steps, otherwise the two records will be flagged as the same group to collapse. In order to compare the cohort, the data provided
#' is sorted by id and dates. Consequently, the output will also be in that order; if joining back to the original data-set, ensure the data is sorted by the provided columns.
#' Since the logic requires looping by individuals, the function is written using \code{data.table}; however, other versions using \code{dplyr} and \code{Rcpp} were trialed as well.
#'
#' @param data A data object (tibble or data.frame).
#' @param grp_id Unique ID for each member of the cohort (unquoted).
#' @param date_start Date format (e.g. YYYY-mm-dd) for entry point for record (unquoted).
#' @param date_end Date format (e.g. YYYY-mm-dd) for exit point for record (unquoted).
#' @param threshold Integer value for acceptable difference in days between successive record (defaults to \code{1}).
#' @param preserve_id Logical value, if set to \code{TRUE} will output list of original ID to ensure column merges back correctly.
#' @return An integer vector (ordered by grp_id and dates) or a list containing the original id and collapse id.
#' @examples
#' # Load libraries
#' library(dplyr); library(data.table); library(lubridate); library(magrittr); library(tibble)
#' # Create fake data for scenarios
#' test_data <- tribble(~grp_id, ~date_start, ~date_end,
#'                      1, '2020-01-01', '2020-01-02',
#'                      1, '2020-01-03', '2020-01-04',
#'                      1, '2020-01-04', '2020-09-02',
#'                      2, '2020-01-01', '2020-09-02',
#'                      2, '2020-09-10', '2020-09-20',
#'                      2, '2020-09-21', '2020-09-22',
#'                      3, '2020-01-01', '2020-01-02',
#'                      3, '2020-01-02', '2020-01-20',
#'                      3, '2020-01-21', '2020-01-22',
#'                      3, '2020-01-22', '2020-04-02',
#'                      3, '2020-04-22', '2021-04-22',
#'                      3, '2021-06-09', '2021-06-22') %>%
#'   dplyr::mutate_at(vars(contains('date')), ymd)
#'
#' # Create vector of outputs (ensure original dataset is sorted)
#' test_data$timestep_group <- collapse_timesteps(data = test_data,
#'                                              grp_id = grp_id,
#'                                              date_start = date_start,
#'                                              date_end = date_end,
#'                                              threshold = 1)
#'
#' test_data %>%
#'   group_by(grp_id, timestep_group) %>%
#'   mutate(min = min(date_start),
#'          max = max(date_end))
#'
#'
#' @importFrom  data.table data.table :=
#'
#' @export
collapse_timesteps <- function(data, grp_id, date_start, date_end, threshold = 1, preserve_id = FALSE){

  # Data checks
  if(missing(data)) stop('Please provide data to the function, either a tibble or data.frame')
  #if(data.table::is.data.table(data)) stop('Provide as tibble or data.frame')
  if(missing(grp_id)) stop('Please provide an ID, a dummy one if only 1 group')
  if(!lubridate::is.Date(data[[deparse(substitute(date_start))]]) || !lubridate::is.Date(data[[deparse(substitute(date_end))]])) stop('Provided date columns must be in a date format.')

  # NSE to SE
  grp_id <- substitute(grp_id)
  date_start <- substitute(date_start)
  date_end <- substitute(date_end)

  # Create data.table and sort
  if(inherits(data, 'data.table')) {
    data <- data.table::copy(data[,c(deparse(grp_id), deparse(date_start), deparse(date_end))])
  } else {
    data <- data.table::as.data.table(data[,c(deparse(grp_id), deparse(date_start), deparse(date_end))])
  }
  data.table::setorderv(data, c(deparse(grp_id), deparse(date_start), deparse(date_end)))

  # Create comparison cols
  data[ , step := data.table::shift(eval(date_start), type = 'lead'),
        by = eval(grp_id)
        ][, step := step - eval(date_end)
          ][ , step_check := step <= threshold
             ][ , progression_check := step_check == data.table::shift(step_check, type = 'lag'),
                by = eval(grp_id)]

  # Use logic to create group
  data[ , `:=`(index = seq_len(.N),
               brk = data.table::fifelse( ((step_check & !progression_check) | (!step_check & progression_check)) , 1, 0, 0)),
        by = eval(grp_id)
        ][ , brk := data.table::fifelse(index == max(index, na.rm = TRUE) & !data.table::shift(step_check, type = 'lag'), 1, brk, 0),
           by = eval(grp_id)
           ][, grp_col := cumsum(brk)+1,
             by = eval(grp_id)]

  # Return vector
  if(preserve_id) {

    temp <- list(data[,eval(grp_id)], data[, grp_col])
    names(temp) <- c(deparse(grp_id), 'collapse_id')
    return(temp)

  } else {
    return(as.integer(data[,grp_col]))
  }
}


#' Assign episode periods for cohorts
#'
#' \code{assign_episode} will create an episode grouping (a vector) for rows in a cohort that are within a threshold difference (usually in days).
#'
#' Data when organized as a cohort will typically have a long-format with multiple entries for an individual monitored over time. Often, subsequent entries between these records are very close in time
#' should be assigned to a episode group. The logic involves comparing the time differences in adjacent entries within each grouping. Based upon the threshold provided and the initial date entry, individuals
#' are rolled-up into episodes that fall within the threshold time interval. In order to compare the cohort, the data provided is sorted by id and date. Consequently, the output will also be in that order; if joining
#' back to the original data-set, ensure the data is sorted by the provided columns. Since the logic requires looping by individuals, the function is written using \code{data.table}.
#'
#' This function is similar to \code{\link{collapse_timesteps}}; however, instead of comparing data formatted in time steps (i.e. with entry and exit dates), \code{assign_episode} operates on
#' data with a single date column reference to determine how to assign individuals to various episode groupings. Where the former may be used to collapse similar time steps, the output
#' from this function will likely be used to analyze differences between and within episode groupings for an individual. If the threshold value needs to change through time, this feature
#' is not directly supported but by sub-setting the data based upon the date ranges the threshold changes, this is possible to include (see example).
#'
#' @param data A data object (tibble or data.frame).
#' @param grp_id Unique ID for each member of the cohort (unquoted).
#' @param date Date format (e.g. YYYY-mm-dd) for entry point for record (unquoted).
#' @param threshold Integer value for acceptable difference in days between successive record (defaults to \code{1}).
#' @param preserve_id Logical value, if set to \code{TRUE} will output list of original ID to ensure column merges back correctly.
#' @return An integer vector (ordered by grp_id and dates) or a list containing the original id and collapse id.
#' @examples
#' # Load libraries
#' library(dplyr); library(data.table); library(lubridate); library(magrittr); library(tibble);
#' # Create fake data for scenarios
#' test_data <- tribble(~grp_id, ~date,
#'                      1, '2020-01-01',
#'                      1, '2020-01-01',
#'                      1, '2020-01-03',
#'                      1, '2020-01-04',
#'                      2, '2020-01-01',
#'                      2, '2020-09-10',
#'                      2, '2020-09-21',
#'                      3, '2020-01-01',
#'                      3, '2020-01-02',
#'                      3, '2020-01-21',
#'                      3, '2020-01-22',
#'                      3, '2020-04-22',
#'                      3, '2021-06-09') %>%
#'   dplyr::mutate_at(vars(contains('date')), ymd)
#'
#' # Create vector of outputs (ensure original dataset is sorted)
#' test_data$episode_group <- assign_episode(data = test_data,
#'                                           grp_id = grp_id,
#'                                           date = date,
#'                                           threshold = 10)
#' # Assign the max/min of episodes
#' test_data %>%
#'   group_by(grp_id, episode_group) %>%
#'   mutate(min = min(date),
#'          max = max(date))
#'
#' # With changing thresholds, assign episodes
#' test_data %>%
#'   mutate(epi_thresh_chg = case_when(date < ymd('2020-01-21') ~ assign_episode(., grp_id, date, threshold =  10),
#'                                     TRUE ~ assign_episode(., grp_id, date, threshold = 100)))
#'
#' @importFrom  data.table data.table :=
#'
#' @export
assign_episode <- function(data, grp_id, date, threshold = 1, preserve_id = FALSE){

  # Data checks
  if(missing(data)) stop('Please provide data to the function, either a tibble or data.frame')
  #if(data.table::is.data.table(data)) stop('Provide as tibble or data.frame')
  if(missing(grp_id)) stop('Please provide an ID, a dummy one if only 1 group')
  if(!lubridate::is.Date(data[[deparse(substitute(date))]])) stop('Provided date columns must be in a date format.')

  # NSE to SE
  grp_id <- substitute(grp_id)
  date <- substitute(date)

  # Create data.table and sort
  if(inherits(data, 'data.table')) {
    data <- data.table::copy(data[,c(deparse(grp_id), deparse(date))])
  } else {
    data <- data.table::as.data.table(data[,c(deparse(grp_id), deparse(date))])
  }
  data.table::setorderv(data, c(deparse(grp_id), deparse(date)))

  # Create comparison and group cols
  data[ , step := data.table::shift(eval(date), type = 'lag'),
        by = eval(grp_id)
        ][, step := eval(date) - step
          ][,zero_check := data.table::fifelse(step == 0, 1, 0, 0)
            ][ , step := {
              temp = as.integer(step);
              .(data.table::fifelse(is.na(temp), 0L, temp))}
              ][, brk := accumulate_threshold(step, threshold),
                by = eval(grp_id)
                ][, grp_col := cumsum(brk==0 & zero_check != 1),
                  by = eval(grp_id)]

  # Return vector
  if(preserve_id) {

    temp <- list(data[,eval(grp_id)], data[, grp_col])
    names(temp) <- c(deparse(grp_id), 'collapse_id')
    return(temp)

  } else {
    return(as.integer(data[,grp_col]))
  }
}

#' Calculate crude rates (internal)
#'
#' @param num Numerator value.
#' @param denom Denominator value
#' @param per Numeric value for rate scale (i.e. per X population).
#'
crude_rate <- function(num, denom, per) {
  if(missing(per)) {
    num / denom
  } else {
    (num / denom) * per
  }
}

#' Calculate standard error (internal)
#'
#' @inheritParams crude_rate
#' @param modified_wald Boolean for if the standard error should be calculated with modified Wald interval.
#'
std_err <- function(num, denom, per, modified_wald = FALSE) {
  if(modified_wald) {
    out <- ((num + 2) / (denom + 4))
  } else {
    crd_rt <- crude_rate(num, denom)
    out <- sqrt( crd_rt * (1 - crd_rt) / denom)
  }

  if(!missing(per)) {
    out <- out * per
    return(out)
  } else {
    return(out)
  }
}


#' Calculate crude rates
#'
#' \code{calculate_cruderate} performs calculations for a crude rate per 100,000 (by default) and associated standard errors. If the numerator is 0, the
#' standard error is determined by the modified Wald interval. Missing numerator values will be filled with zero. When denominator is NA,
#' the rates/SE will not occur.
#'
#' @param data A data object.
#' @param numerator Character vector of the column name.
#' @param denominator Character vector of the column name.
#' @param per Numeric value for rate scale (i.e. per X population).
#' @return A new dataset (\code{tibble}) with crude rate and standard error added.
#'
#' @examples
#' tempData <- data.frame(cases = as.numeric(c(1, 0 , 100, 10, NA)), population = as.numeric(c(10, 1000, 3200, NA, 1000)))
#' calculate_cruderate(tempData, 'cases', 'population')
#'
#' @export
calculate_cruderate <- function(data, numerator, denominator, per = 100000) {
  BLANK <- rep(NA, nrow(data))
  CRUDERATE <- BLANK # Initialize the new column
  STNDERR <- BLANK
  data <- as.data.frame(data) # For faster calculations and formatting

  if(any(data[[denominator]] < data[[numerator]], na.rm = T)) warning('Denominator is less than numerator')
  if(any(is.na(data[[numerator]]))) warning('NA values for numerator present, will be filled with 0')
  if(any(is.na(data[[denominator]]))) warning('NA values for denominator present, rates will be left as NA')
  if(!is.numeric(per) || per < 1) stop('Invalid value for `per`, provide a number greater than 1.')


  # Fill any missing numerator with 0
  data[[numerator]] <- dplyr::if_else(is.na(data[[numerator]]), 0, data[[numerator]])

  # Calculate statistics
  CRUDERATE <- crude_rate(data[[numerator]], data[[denominator]], per)
  STNDERR <- std_err(data[[numerator]], data[[denominator]], per)
  STNDERR <-  dplyr::if_else(!is.na(STNDERR) & STNDERR == 0,
                             std_err(data[[numerator]], data[[denominator]], per, modified_wald = TRUE),
                             STNDERR)

  # Assign and return
  data$CRUDERATE <- CRUDERATE
  data$STNDERR <- STNDERR
  return(tibble::as_tibble(data))
}


#' Round values in select columns
#'
#' \code{round_cols} will round values in a pre-defined selection of columns in a dataset.
#'
#' The function is vectorized so that multiple inputs can be examined without having to define any specific loops.
#'
#' @param data A data object.
#' @param col_list A character vector of specific column names to round.
#' @param dec A numeric value to control precision of rounding.
#' @return A new dataset object with select columns rounded.
#'
#' @examples
#' tempData <- data.frame(rate = as.numeric(c(10.344, 0.359 , 1023.49)), value = as.numeric(c(10, 1000, 3200.2)))
#' round_cols(tempData, c('rate'), 2)
#'
#' @export
round_cols <- function(data, col_list, dec = 2) {
  data[, (names(data) %in% col_list)] <- data %>%
    dplyr::select_if(colnames(data) %in% col_list) %>%
    purrr::map(~round(., digits = dec))

  return(data)
}

#' Calculate age
#'
#' \code{calculate_age} will determine age in years based upon two comparison dates.
#'
#' Rounding is performed with \code{floor} so you are the same age up to the day before the next birthday
#' (e.g 5 years old from 5th birthday through the day before your 6th birthday). Set \code{floor = FALSE} to
#' return decimal ages, and change \code{units} for units other than years. Try combining with \code{\link{create_breaks}}
#' to make age groupings.
#'
#' @param dob Vector containing date of birth for age calculation.
#' @param age_day Reference date to calculate age as of that time-point.
#' @param units unit to measure age (default set to \code{"years"}). Passed to \code{\link[lubridate]{duration}}.
#' @param floor Boolean to determine to floor round the result (default set to \code{TRUE}).
#' @return Age in \code{units}. Will be an integer if \code{floor = TRUE}.
#' @author
#' Gregor Thomas \url{https://stackoverflow.com/users/903061/gregor-thomas>}
#' @note Adapted from SO post. Earlier methods used \code{as.integer((compare_date - dob) / 365.25)}
#' @source \url{https://stackoverflow.com/questions/27096485/change-a-column-from-birth-date-to-age-in-r}
#' @examples
#' \dontrun{
#' tempData <- data.frame(date_of_birth = as.Date(c('1991-01-01', '1990-02-04')), death_date = as.Date(c('1992-01-1', '2020-01-01')))
#' calculate_age(tempData$date_of_birth, tempData$death_date)
#' calculate_age(tempData$date_of_birth, tempData$death_date, units = 'minutes')
#' calculate_age(tempData$date_of_birth, tempData$death_date, floor = FALSE)
#' }
#' @export

calculate_age <- function(dob, age_day = Sys.Date(), units = "years", floor = TRUE) {
  age = lubridate::interval(dob, age_day) / lubridate::duration(num = 1, units = units)

  if (floor) return(as.integer(floor(age)))

  return(age)
}


#' Determine the top max or min values
#'
#' Determine the top maximum or minimum values in a vector.
#'
#' @param x A data object.
#' @param N Character vector of the column name.
#' @return The Nth max or min value (numeric).
#'
#' @examples
#' tempData <- c(1,4,1,7,2)
#' maxN(tempData, N=2)
#' minN(tempData, N=2)
#'
#' @source Adapted from \href{https://stackoverflow.com/users/345660/zach}{Zachary Mayer's} stackoverflow contribution:
#' \url{https://stackoverflow.com/questions/2453326/fastest-way-to-find-second-third-highest-lowest-value-in-vector-or-column}
#'
#' @export
maxN <- function(x, N=1){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x). Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=len-N+1)[len-N+1]
}

#' @rdname maxN
#' @export
minN <- function(x, N=1){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=N)[N]
}

#' Round up to a 'nice' number.
#'
#' \code{round_up_nice} will examine a vector of numbers and round each upwards to a 'nice' value.
#'
#' The function is vectorized so that multiple inputs can be examined without having to define any specific loops.
#'
#' When defining a vector of numeric values for \code{nice}, one must consider that these values will be used to determine the extend the number is rounded up.
#' For example, if the number to round is 10.1, with the default \code{nice}, this will round to 20. This is determined by taking the base-10 log of the input value
#' raising it to the power of 10 and then multiplying that value by each \code{nice} value; the value selected is the lowest of which is bigger than the input. The
#' calculation would follow as such:
#' \deqn{10^floor(log10(10.1)) = 1}
#' \deqn{10^1 = 10}
#' \deqn{10 * (1,2,4,5,6,8,10) = 10  20  40  50  60  80 100}
#' So, 20 would be selected as it is the closest value in this range that is greater than 10.1. Adjust the \code{nice} to suit the situation.
#'
#' @param x A data object.
#' @param nice A numeric vector defining base values that the user believes are 'nice'.
#' @return A numeric vector with all numbers rounded up nicely.
#'
#' @examples
#' round_up_nice(x=c(1,4,5,10.1,55.5,60.2, 1001.3), nice = c(1,2,4,14))
#'
#' @source Adapted from \href{https://stackoverflow.com/users/662787/tommy}{Tommy's} StackOverflow contribution:
#' \url{https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x}
#'
#' @seealso \code{\link[base]{pretty}}
#'
#' @export
round_up_nice <- function(x, nice=seq(1,10)) {

  if(!is.numeric(x)) stop("'x' should be a numeric value.")
  scale_x <- 10^(floor(log10(x)))
  purrr::map2_dbl(x, scale_x, ~.y * nice[[which(.x <= .y * nice)[[1]]]])

}
