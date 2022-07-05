#' Create data with multilevel factor categories
#'
#' \code{create_multilevel_factor} will append additional rows to the provided dataset based upon new factor levels, provided as a
#' named list to the function. This is based upon \code{MULTILABEL FORMAT} from \code{SAS}, which allows various overlapping categories
#' to be applied to the same observations, making it easy to tally various combinations of otherwise granular factor levels.
#'
#' There are some limitations, the function has not been tested when converting between a numerical data (e.g. ages) to various overlapping
#' age groups. Furthermore, the name of the list to label a new category is not vectorized. One needs to be careful of using the 'group' parameter,
#' as it will be examine existing levels within that group before deciding how to apply a new label. This works well for seeing which levels exist, but
#' less so to check which do not exist. The grouping function requires that a single column can define the entire grouping, so columns may need to be
#' combined to ensure the right comparison is made.
#'
#' @param data A dataset, preferably as a tibble.
#' @param target_col A character vector representing the column of interest to add new factor levels.
#' @param new_levels A named list provided to aggregate existing factor levels into additional combined levels.
#' @param group_col A character vector for the column that will group observations.
#' @param collapse Logical value to determine if new rows should be collapsed as unique combinations.
#' @param track Logical value to determine if a new column should be added to track the added factor levels by their rows.
#' @return A new dataset with additional rows for the added factor categories.
#' @examples
#' library(tibble)
#' # Example data (Repeat groups)
#' exampleData <- tibble(group = c(1, 1, 1, 2, 3, 3),
#'                       condition = factor(c('A', 'B', 'C', 'A', 'B', 'Q'), ordered = FALSE))
#'
#' # With grouping
#' newData <- create_multilevel_factor(exampleData,
#'                                    target_col = 'condition',
#'                                    group_col = 'group',
#'                                    new_levels = list('AB' = c('A', 'B'), 'QB' = c('Q', 'B')),
#'                                    collapse = TRUE, track = TRUE)
#'
#' newData
#' addmargins(table(newData$group,newData$condition))
#'
#' # Without grouping
#' newData2 <- create_multilevel_factor(exampleData,
#'                                    target_col = 'condition',
#'                                    new_levels = list('AB' = c('A', 'B'), 'QB' = c('Q', 'B')),
#'                                    collapse = TRUE, track = TRUE)
#'
#' newData2
#'
#'
#' @export

create_multilevel_factor <- function(data, target_col, new_levels , group_col, collapse = TRUE, track = TRUE) {

  # Checks the input #

  # Check if new_levels is provided as a list
  if(!is.list(new_levels)) stop('The provided set of levels is not in a list format, please provide as a list')

  # Check if target_col is a factor
  if(!is.factor(data[[target_col]])) stop('The target column for multiple levels is not a factor, convert to a factor before proceeding.')

  # Check if levels are in list
  for(i in 1:length(new_levels)) {
    if(length(setdiff(levels(factor(new_levels[[i]])),
                      levels(factor(data[[target_col]])))) > 0) { # If levels in provided list contain a level not in the column, then throw error
      stop('Levels in list do not match the levels in the target column')
    }
  }

  # State if grouping col was provided and its purpose
  if(!missing(group_col)) { message(paste0('The following column is used as a grouping variable for summarizing multilevel factoring: ',
                                           group_col, '. If you do not want labels determined by those within groupings, leave argument blank.'))
  }

  # Main operations #

  # Set new column for tracking if desired
  if(track) {track_col <- rep(NA,nrow(data)); data$track_col <- 1;  trackColIndex <- 1;}

  OutData <- as.data.frame(NULL) # Empy data frame to fill and append later

  # Loop for all new levels of interest to add
  for(i in 1:length(new_levels)){

    tempData <- data # Look at fresh data every pass

    levelIndex <- which(levels(tempData[[target_col]]) %in% new_levels [[i]]) # Index of matches

    # If grouping provided, do necessary splits and rbinds
    if(!missing(group_col)) {
      tempData <- split(tempData, tempData[[group_col]]) # Split if there are groupings

      tempData <- lapply(tempData, function(x) {
        if(!(length(setdiff(levels(factor(new_levels [[i]])), levels(factor(x[[target_col]])))) > 0)) { # If the grouping does not have all the levels for the new grouping, then do nothing
          levels(x[[target_col]])[levelIndex] <- names(new_levels)[i]
          x
        }
      })

      tempData <- do.call(rbind, tempData)  # If didnt match necessary group conditions, will bring back empty
      rownames(tempData) <- NULL # Correct row names for tibble

    } else { # If not grouping
      levels(tempData[[target_col]])[levelIndex] <- names(new_levels)[i]

    }

    tempData <- tempData[tempData[[target_col]] %in% names(new_levels )[i],] # Only keep new factor levels (could be empty if no group matches)

    if(collapse) tempData <- unique(tempData[(tempData[[target_col]] %in% names(new_levels)[i]),]) # Collapse to unique combinations if desired

    if(track){track_col <- rep(NA, nrow(tempData));  tempData$track_col <- trackColIndex+1;  trackColIndex <- trackColIndex+1;} # Add track column to the new rows

    OutData <- suppressWarnings(dplyr::bind_rows(OutData, tempData)) # Append all the new rows
  }

  # Append new rows to the original rows
  OutData <- suppressWarnings(dplyr::bind_rows(data, OutData))

  return(OutData)

}

#' Create a data hypercube
#'
#' \code{create_hypercube} creates a data cube from a flat data table. This is very similar to \code{groupingsets()} in the \code{data.table} package.
#'
#' If \code{drop_na_levels} is set to \code{TRUE} all factor levels which are \code{NA} will be removed. A label can also be set for all the 'sum' levels;
#' typically these 'sum' levels are \code{NA_character_} but they will not be removed by \code{drop_na_levels} (only NA levels that exist in the data prior to the summarizing
#' are dropped.
#'
#' @param data Data object.
#' @param columns Character vector, must be more than 1 column provided.
#' @param drop_sum_columns Character vector of which columns to drop the 'sum' levels.
#' @param use_na Character vector regarding if \code{NA} levels should be included in tallies, passed to \code{table}.
#' @param drop_na_levels Logical vector, if \code{TRUE} all \code{NA} factor levels are dropped.
#' @param label_for_sum Character vector, a label to replace the default text of 'sum' for column totals.
#' @return A \code{data.frame} or \code{tibble} is returned
#' @examples
#' create_hypercube(mtcars, columns = c('gear', 'cyl'), drop_sum_columns = 'cyl')
#'
#' @export
create_hypercube <- function(data, columns, drop_sum_columns = "", use_na = "ifany", drop_na_levels = TRUE, label_for_sum = NULL){

  message("It is recommended to have categorial data set as factors with desired labelling before running this function")

  # Which of the columns selected are factors upon starting
  index <- NULL
  index_c <- NULL
  data <- tibble::as_tibble(data)
  for (i in columns){
    if(any(class(data[[i]])=="factor")){
      index <- c(index, i)
    }
    if(any(class(data[[i]])=="character")){
      index_c <- c(index_c, i)
    }
  }

  data <- tibble::as_tibble(stats::addmargins(table(data[,columns], useNA = use_na, dnn = columns))) # By default, includes NA in totals; dnn to ensure even 1 dimension can be done

  if(drop_na_levels==T){
    # Filter out all the missing
    for(i in columns){
      data <- dplyr::filter(data, !is.na(data[i]))
    }
  }

  # Filter out the summary columns!
  if(all(drop_sum_columns!= "")){
    for(i in drop_sum_columns){
      data <- dplyr::filter(data, data[i] != "Sum")
    }
  }

  # For any sum columns remaining, convert them to NAs
  if(!is.null(label_for_sum)){
    # Turn all 'sum' from the table function to NA so correct factors are unused
    for(i in columns){
      data[[i]] <- dplyr::if_else(data[[i]] == "Sum", label_for_sum, data[[i]])
    }
  }

  # General column specification
  data <- suppressMessages(readr::type_convert(data))

  # Based upon index, change those columns to factors!
  if(!is.null(index)){
    data[index] <- purrr::map(data[index], factor)
  }

  if(!is.null(index_c)){
    data[index_c] <- purrr::map(data[index_c], as.character)
  }

  message("Datacube completed, you will need to rename the label for `Sum` in your data levels")

  return(data)
}

#' Create a data list
#'
#' \code{create_datalist} creates data list by using data names and combining them all into a named list.
#'
#' @param data_names A character vector of all datasets in the selected environment to collapse into a named list.
#' @param envir An environment object, defaulted to \code{.GlobalEnv}
#' @return A named list.
#' @examples
#' dataset1 <- mtcars
#' dataset2 <- iris
#' combined_datalist <- create_datalist(c('dataset1', 'dataset2'))
#'
#' @export
create_datalist <- function(data_names, envir = .GlobalEnv) {
  templist <- mget(ls(envir = envir)[ls(envir = envir) %in% data_names], envir = envir)
  return(templist)
}

#' Create middle labels with 'precision format' (internal)
#'
#' @inheritParams create_breaks
#' @param brk_length Length of number of breaks provided to parent function.
#'
#' @seealso \code{\link{create_breaks}}
label_notation_precision <- function(breaks, brk_length, precision, divider, left.open, rightmost.closed) {

  if(all(left.open, rightmost.closed)) { #TT
    upper_sym <- '>'
    lower_sym <- '<'
    middle_labels <- c(paste0(breaks[1], divider, breaks[2]), paste0(breaks[2:(brk_length-1)] + precision, divider, breaks[3:brk_length]))

  }  else if(left.open && !rightmost.closed) { #FF
    upper_sym <- '>'
    lower_sym <- '<='
    middle_labels <- c(paste0(breaks[1], divider, breaks[2]), paste0(breaks[2:(brk_length-1)] + precision, divider, breaks[3:brk_length]))

  } else if(!left.open && rightmost.closed) { #TF
    upper_sym <- '>'
    lower_sym <- '<'
    lb <- breaks[1:(brk_length-2)]
    ub <- breaks[2:(brk_length-1)] - precision
    index <- purrr::map2_lgl(lb, ub, ~`==`(.x,.y))
    middle_labels <- character(length(index)+1) # +1 for the top bracket that is inclusive
    middle_labels[which(index)] <- paste0(lb[index])
    middle_labels[which(!index)] <- paste0(lb[!index], divider, ub[!index])
    middle_labels[length(index)+1] <- paste0(breaks[(brk_length-1)], divider, breaks[(brk_length)])

  } else if(all(!left.open, !rightmost.closed)) { #FT
    upper_sym <- '>='
    lower_sym <- '<'
    lb <- breaks[1:(brk_length-1)]
    ub <- breaks[2:(brk_length)] - precision
    index <-  purrr::map2_lgl(lb, ub, ~`==`(.x,.y))
    middle_labels <- character(length(index)) # All the middle labels get changed as top not inclusive
    middle_labels[which(index)] <- paste0(lb[index])
    middle_labels[which(!index)] <- paste0(lb[!index], divider, ub[!index])
  }
  return(list(upper_sym = upper_sym,
              lower_sym = lower_sym,
              middle_labels = middle_labels))
}

#' Create middle labels with 'bracket format' (internal)
#'
#' @inheritParams create_breaks
#' @param brk_length Length of number of breaks provided to parent function.
#'
#' @seealso \code{\link{create_breaks}}
label_notation_brackets <- function(breaks, brk_length, divider, left.open, rightmost.closed) {
  if(all(left.open, rightmost.closed)) { #TT
    upper_sym <- '>'
    lower_sym <- '<'

    middle_labels <- paste0(breaks[1:(brk_length-1)], divider, breaks[2:brk_length])
    middle_labels[2:(brk_length-1)] <- paste0('(', middle_labels[2:(brk_length-1)], ']') # interior
    middle_labels[1] <- paste0('[', middle_labels[1], ']') # first edge

  } else if(left.open && !rightmost.closed) { #FF
    upper_sym <- '>'
    lower_sym <- '<='

    middle_labels <- paste0(breaks[1:(brk_length-1)], divider, breaks[2:brk_length])
    middle_labels <- paste0('(', middle_labels, ']')

  } else if(!left.open && rightmost.closed) { #TF
    upper_sym <- '>'
    lower_sym <- '<'
    lb <- breaks[1:(brk_length-2)]

    ub <- breaks[2:(brk_length-1)]
    index <- purrr::map2_lgl(lb, ub, ~`==`(.x,.y))
    middle_labels <- character(length(index)+1) # +1 for the top bracket that is inclusive
    middle_labels[which(index)] <- paste0(lb[index])
    middle_labels[which(!index)] <- paste0(lb[!index], divider, ub[!index])
    middle_labels[length(index)+1] <- paste0(breaks[(brk_length-1)], divider, breaks[(brk_length)])

    middle_labels[1:(brk_length-2)] <- paste0('[', middle_labels[1:(brk_length-2)], ')')
    middle_labels[brk_length-1] <- paste0('[', middle_labels[brk_length-1], ']')

  } else if(all(!left.open, !rightmost.closed)) { #FT
    upper_sym <- '>='
    lower_sym <- '<'

    lb <- breaks[1:(brk_length-1)]
    ub <- breaks[2:(brk_length)]
    index <-  purrr::map2_lgl(lb, ub, ~`==`(.x,.y))
    middle_labels <- character(length(index)) # All the middle labels get changed as top not inclusive
    middle_labels[which(index)] <- paste0(lb[index])
    middle_labels[which(!index)] <- paste0(lb[!index], divider, ub[!index])

    middle_labels <- paste0('[', middle_labels, ')')
  }

  return(list(upper_sym = upper_sym,
              lower_sym = lower_sym,
              middle_labels = middle_labels))

}

#' Create middle labels with 'sign format' (internal)
#'
#' @inheritParams create_breaks
#' @param brk_length Length of number of breaks provided to parent function.
#'
#' @seealso \code{\link{create_breaks}}
label_notation_signs <- function(breaks, brk_length, precision, divider, left.open, rightmost.closed) {

  if(all(left.open, rightmost.closed)) { #TT
    upper_sym <- '>'
    lower_sym <- '<'

    middle_labels <- c(paste0(breaks[1], divider, breaks[2]), # First edge
                       paste0('>', breaks[2:(brk_length-1)], divider, breaks[3:brk_length])) # interior


  } else if(left.open && !rightmost.closed) { #FF
    upper_sym <- '>'
    lower_sym <- '<='

    middle_labels <- paste0('>', breaks[1:(brk_length-1)], divider, breaks[2:brk_length])

  } else if(!left.open && rightmost.closed) { #TF
    upper_sym <- '>'
    lower_sym <- '<'
    lb <- breaks[1:(brk_length-2)]

    ub <- breaks[2:(brk_length-1)]
    index <- purrr::map2_lgl(lb, ub, ~`==`(.x,.y))
    middle_labels <- character(length(index)+1) # +1 for the top bracket that is inclusive
    middle_labels[which(index)] <- paste0(lb[index])
    middle_labels[which(!index)] <- paste0(lb[!index], divider, '<',ub[!index]) # Sign here...
    middle_labels[length(index)+1] <- paste0(breaks[(brk_length-1)], divider, breaks[(brk_length)])

  } else if(all(!left.open, !rightmost.closed)) { #FT
    upper_sym <- '>='
    lower_sym <- '<'

    lb <- breaks[1:(brk_length-1)]
    ub <- breaks[2:(brk_length)]
    index <-  purrr::map2_lgl(lb, ub, ~`==`(.x,.y))
    middle_labels <- character(length(index)) # All the middle labels get changed as top not inclusive
    middle_labels[which(index)] <- paste0(lb[index])
    middle_labels[which(!index)] <- paste0(lb[!index], divider, '<', ub[!index])
  }

  return(list(upper_sym = upper_sym,
              lower_sym = lower_sym,
              middle_labels = middle_labels))

}

#' Create breaks based on numeric input
#'
#' Break continuous values into bins and apply clean labeling. Wraps around \code{\link{findInterval}} and \code{\link{factor}} functions.
#'
#' For edge cases, ensure that labeling is suitable. For example, if you do not want the upper boundaries included then adjustments must be made to the
#' values sent to \code{findInterval} via \code{...}. When providing custom labels to the \code{format} parameter ensure you have compensated for a label that
#' may sit outside the typical boundary of your provided breaks (e.g. <0 if you lowest break is 0 but your data can take on negative values).
#' If this function is not providing what you require, try looking at \code{\link{cut}}. If you need the ability to automatically cut groups into defined sizes
#' look at the \code{{classInt}} or provide \code{\link{cut}} a single value for the number of equal breaks to create. \code{create_breaks} also has a formatting
#' option to use bracket notation for the middle bounds, this may be preferred if the default automatic formatting with assigned precision is not to one's liking.
#'
#' Adjusting the \code{precision} affects the rounding precision of the labels. By default it will use the smallest decimal place in the parameter \code{brks}.
#' Depending on use-case, it may be important to ensure your binning is occurring as expected (e.g. that partial ages like 5.4 yo bins in 4-5 or 5-6). Rounding prior to
#' using this function may help avoid such issues.
#'
#' @param x Numeric vector to create break groupings from.
#' @param breaks Numeric vector of breakpoints supplied to \code{\link{findInterval}}.
#' @param format Provide your own labels; if set to \code{TRUE} the clean labeling will be set automatically.
#' @param precision Numeric value, determines how to adjust boundary of labels. Default will determine from provided breaks (e.g. 1 decimal place means precision of 0.1).
#' @param divider Character defining the symbol separating adjacent label values (default is a dash).
#' @param left.open Logical; passed to \code{\link{findInterval}.
#' @param rightmost.closed Logical; passed to \code{\link{findInterval}}.
#' @param format_notation Character vector to determine label style; valid inputs include 'precision', 'brackets', or 'signs' (only the first uses the precision parameter).
#' @param ... Additional parameters supplied to \code{\link{findInterval}}
#'
#' @return Vector with assignment for each grouping (numeric if no format provided, factor when format provided)
#'
#' @export
#' @examples
#'
#' # Data setup
#' data = c(-1,0,10,5,999,9)
#' breaks = c(0, 1, 10,50,100)
#' labels = c('<0', '0-1', '1-10', '10-50', '50-100', '100+')
#'
#' #If many break labels, try using rep() or seq(), and paste them in interation
#' labels2 = purrr::map2_chr(seq(10, 99, 10),  seq(20, 100, 10)-1, ~paste0(.x, '-', .y))
#'
#' # Create break without any formatting
#' breaks_numeric <- create_breaks(data, breaks)
#'
#' # Create break with default label formatting
#' breaks_auto <- create_breaks(data, breaks, format = TRUE)
#'
#' # Create break with custom label formatting
#' breaks_custom <- create_breaks(data, breaks, format = labels)
#'
#' # Create breaks without any precision (will see start/end of categories as same number)
#' create_breaks(data, breaks, format = labels, precision = 0)
#'
#' # Cut function as alternative
#' cut(data, breaks)
#'
#' # Cut function fills NA if you dont define -Inf and +Inf in the breaks
#' # ... also has less auto-formatting abilities (index only, bracket notation or custom)
#' cut(data, c(-Inf, 0, 1, 10,50,100, Inf))
#' cut(data, c(-Inf, 0, 1, 10,50,100, Inf), labels = FALSE) # Only index the group, not auto-label
#'
#' # Compare various outputs
#' x <- c(0.4, 0.6, 1:10)
#' v <- c(0.5, 5, 7, 9)
#' cbind(x,
#' TF=as.character(create_breaks(x, v, left.open =T, rightmost.closed = F, format = T,precision = 0)),
#' FF=as.character(create_breaks(x, v, left.open =F, rightmost.closed = F, format = T,precision = 0)),
#' TT=as.character(create_breaks(x, v, left.open =T, rightmost.closed = T, format = T,precision = 0)),
#' FT=as.character(create_breaks(x, v, left.open =F, rightmost.closed = T, format = T,precision = 0)),
#' TF2=as.character(create_breaks(x, v, left.open =T, rightmost.closed = F, format = T, format_notation = 'brackets')),
#' FF2=as.character(create_breaks(x, v, left.open =F, rightmost.closed = F, format = T, format_notation = 'brackets')),
#' TT2=as.character(create_breaks(x, v, left.open =T, rightmost.closed = T, format = T, format_notation = 'brackets')),
#' FT2=as.character(create_breaks(x, v, left.open =F, rightmost.closed = T, format = T, format_notation = 'brackets')))
create_breaks <- function(x, breaks, format = FALSE, precision, divider = '-', left.open = FALSE, rightmost.closed = FALSE, format_notation = 'precision', ...) {

  # Error checking and stops
  stopifnot(is.numeric(x), is.numeric(breaks))
  message('Break intervals were: ', paste(breaks, collapse = ' '))
  format_notation <- match.arg(format_notation, choices = c('precision', 'brackets', 'signs'))
  if(format_notation != 'precision') message('Based on format_notation selection, the precision parameter will be ignored')

  # Ensure in correct order!
  breaks <- sort(breaks)

  # Take max decimal places if no precision set
  if(missing(precision)) {
    max_brk_dec <- max(count_decimals(breaks))

    if(max_brk_dec==0) {
      precision <- 1 # If no decimals, default to 1's place
    } else if(is.infinite(max_brk_dec)) {
      message('Break maximum is infinite')
      precision <- 1
    } else {
      precision <- 1 * (10 ^ -(max_brk_dec)) # so 1 = 0.1
    }

    message('Precision set to: ', precision)
  }
  if(precision == 0) {
    warning('Precision set to: 0. Be careful interpreting label boundaries; providing precision or using bracket notation parameter will give more explicit labels')
  }

  # Create the initial interval based on provided details
  out <- findInterval(x, breaks, left.open = left.open, rightmost.closed = rightmost.closed, ...)
  out_length <- length(out)
  brk_length <- length(breaks)
  if(brk_length<3) warning('Having less than 3 breaks may result in odd labelling; override with manual provided breaks')

  # Logic checks based on intervals that affect subsequent naming
  if(brk_length>=3){
    label_list <- switch(format_notation,
                         'precision' = label_notation_precision(breaks, brk_length, precision = precision, divider = divider, left.open = left.open, rightmost.closed = rightmost.closed),
                         'brackets' = label_notation_brackets(breaks, brk_length, divider = divider, left.open = left.open, rightmost.closed = rightmost.closed),
                         'signs' = label_notation_signs(breaks, brk_length, divider = divider, left.open = left.open, rightmost.closed = rightmost.closed))

    upper_sym <- label_list$upper_sym
    lower_sym <- label_list$lower_sym
    middle_labels <- label_list$middle_labels
    rm(label_list)

  } else { #TODO add more helpful autolabelling when only 2 breaks provided
    if(format_notation != 'precision') warning('Bracket and sign notation not supported when 2 or less break points')
    upper_sym <- '>'
    lower_sym <- '<'
    middle_labels <- paste0(breaks[1:brk_length-1], divider, breaks[2:brk_length])
  }

  # Assigning labels to breaks
  if(is.logical(format) && format) {

    brk_labels <- vector()

    # Possible bottom label
    if(min(out, na.rm = TRUE) == 0) {
      brk_labels <- paste0(lower_sym, breaks[1])
    }

    # Middle labels
    brk_labels <- c(brk_labels, middle_labels)

    # Possible top label
    if(max(out, na.rm = TRUE) == brk_length){ # Due to brk groups having 1 less then vector, they will be the same here
      brk_labels <- c(brk_labels, paste0(upper_sym, breaks[brk_length]))
    }

    levels_out <- seq(min(out, na.rm = TRUE), max(out, na.rm = TRUE))
    corrected_lbl_length <- brk_labels[min(out, na.rm = TRUE):max(length(levels_out), max(out, na.rm = TRUE))] # To ensure they align to labels

    out <- factor(out, levels = levels_out, labels = corrected_lbl_length)

    # If applying custom labels
  } else if (is.character(format)) {

    levels_out <- seq(min(out, na.rm = TRUE), max(out, na.rm = TRUE))
    corrected_lbl_length <- format[min(out, na.rm = TRUE):max(length(levels_out), max(out, na.rm = TRUE))]

    out <- factor(out, levels = levels_out, labels = corrected_lbl_length)
  }

  return(out)
}

#' Create clean integer breaks for scales
#'
#' \code{create_pretty_int_brks} will take an input of values and generate clean integer break points,
#' scaled by a multiplier variable.
#'
#' @param x Data vector.
#' @param max_multiplier Integer vector to control the scaling of the breaks.
#'
#' @examples
#' data_for_breaks <- c(1,2,3.8,5,25.5,60)
#' create_pretty_int_brks(data_for_breaks, max_multiplier = 1.05)
#'
#' @export
create_pretty_int_brks <- function(x, max_multiplier = 1.03){
  unique(floor(pretty(seq(0, (max(x) + 1) * max_multiplier))))
}

#' Create x,y coordinates of a circle
#'
#' \code{createCircle} will create the x,y coordinates for a circle to be made using the polygon geom in \code{ggplot2}. This code is
#' internal to \code{\link{StatBullet}}.
#'
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @author Bob Rudis
#' @source Adapted from \href{https://stackoverflow.com/users/1457051/hrbrmstr}{Bob Rudis'} StackOverflow post:
#' \url{https://stackoverflow.com/questions/31635732/stacked-bubble-chart-bottom-aligned}
create_circle <- function(center, radius) {
  th <- seq(0, 2*pi, len=200) # Resolution of edges
  data.frame( # Add group as parameter if needed...
    x=center[1] + radius*cos(th), # Create x points using cos
    y=center[2] + radius*sin(th)) # Create y points from sine
}
