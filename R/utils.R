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
                        nchar(strsplit(str, "\\.")[[1]][2])
                      },
                      numeric(1))

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
