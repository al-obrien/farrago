#' ggproto of geom_xspline for ggplot2
#'
#' \code{GeomXSpline} is the \code{ggproto} object required for \code{\link{geom_xspline}}.
#'
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @export
GeomXSpline <- ggplot2::ggproto("GeomXSpline", ggplot2::Geom,
                                required_aes = c("x", "y"),
                                default_aes = ggplot2::aes(colour = "black",
                                                           size = 0.5,
                                                           linetype = 1,
                                                           alpha = 1,
                                                           spline_shape=-1,
                                                           open=T),

                                draw_key = ggplot2::draw_key_smooth, # controls what is drawn in legend

                                draw_group = function(data, panel_params, coord) {

                                  n <- nrow(data)
                                  if (n <= 2) return(grid::nullGrob())

                                  coords <- coord$transform(data, panel_params)

                                  first_row <- coords[1, , drop = FALSE]

                                  grid::xsplineGrob(
                                    coords$x, coords$y,
                                    shape = coords$spline_shape,
                                    open = coords$open[1],
                                    gp = grid::gpar(col = first_row$colour,
                                                    lwd = first_row$size * .pt,
                                                    alpha = first_row$alpha,
                                                    lty = first_row$linetype)
                                  )
                                }
)

#' Xspline geom for ggplot2
#'
#' Based upon \code{\link{xsplineGrob}} in \code{base R}, \code{geom_xspline} draws an X-spline using control points. The end result is a smooth line that can be used
#' as a replacement to \code{\link[ggplot2]{geom_line}}.
#'
#' This code was written since \code{\link[ggalt]{geom_xspline}} did not operate well with \code{\link[ggpubr]{ggarrange}} at the time.
#'
#' @inheritParams ggplot2::geom_line
#' @param spline_shape A numeric value between -1 and 1 (shape of spline to control points \code{\link[ggalt]{geom_xspline}}).
#' @param open A logical value for if the X-spline has open or closed shape.
#'
#'
#' @source Adjusted from \code{\link[ggalt]{geom_xspline}} by Bob Rudis (hrbrmstr): \url{https://github.com/hrbrmstr/ggalt/blob/master/R/geom_xspline2.r}.
#'
#' @references Blanc, C. and Schlick, C. (1995), "X-splines : A Spline Model
#'             Designed for the End User", in \emph{Proceedings of SIGGRAPH 95},
#'             pp. 377-386. \url{http://dept-info.labri.fr/~schlick/DOC/sig1.html}
#'
#' @seealso Extending ggplot2: \url{https://ggplot2.tidyverse.org/articles/extending-ggplot2.html};
#' Blog post on custom ggplot2 functions by hrbrmstr: \url{https://rud.is/b/2015/09/08/roll-your-own-stats-and-geoms-in-ggplot2-part-1-splines/};
#' \code{\link[graphics]{xspline}};
#' \code{\link[grid]{grid.xspline}}.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(magrittr)
#'
#' # Create plot and then use ggarrange to merge the two
#' ggplot(data = mtcars, aes(x = wt, y = mpg)) +
#'   geom_xspline(spline_shape = -.25) +
#'   geom_point()
#'
#' ggpubr::ggarrange(myplot, myplot)
#' }
#' @export
geom_xspline <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         spline_shape = -1,
                         open = TRUE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {
  ggplot2::layer(
    geom = GeomXSpline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(spline_shape = spline_shape,
                  open = open,
                  na.rm = na.rm,
                  ...)
  )
}

#' Add a common legend with grid.arrange
#'
#' Plot several separate plots into a combined figure with a shared legend using grobs and \code{\link[gridExtra]{grid.arrange}}.
#'
#' \code{grid.arrange} can be more flexible for arranging grobs and plots of different types. If additional parameters are desired,
#' the function may need to be altered. For example, more legend positions could be added to the switch function.
#'
#' Furthermore, the function assumes that every plot has the same amount of levels for hte factor. If some categories are missing from a plot,
#' ensure the correct plot reference is selected and when making each plot, ensure the legend includes categories that are not plotted. See the
#' example below for some more guidance.
#'
#' @param ... Plot objects.
#' @param nrow Integer, number of rows for plotting objects.
#' @param ncol Integer, number of columns for plotting objects.
#' @param position Character vector passed to \code{theme} from \code{\link[ggplot2]{theme}}.
#' @param legend_reference An integer that references the plot, in the order provided, to determine where the shared legend
#' should originate from; this is defaulted to 1.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#' library(magrittr)
#'
#' data1 <- iris %>% filter(Species == 'setosa') # Levels not dropped
#' data2 <- iris %>% filter(Species != 'setosa') # Levels not dropped
#' levels(data1$Species)
#' levels(data2$Species)
#'
#' plot1 <- ggplot(data1, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
#'            geom_point() +
#'            scale_color_discrete(drop = FALSE) # Ensure that the legend shows all the categories for color!
#' plot2 <- ggplot(data2, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
#'            geom_point() +
#'            scale_color_discrete(drop = FALSE)
#'
#' grid_arrange_shared_legend(plot1,plot2, legend_reference = 1) # Since both plots have the same legend, reference can be either 1 or 2
#'}
#' @source Adapted from \href{https://stackoverflow.com/users/4581430/steven-lockton}{Steven Lockton's}
#' \href{https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots}{StackOverflow contribution} and
#' \href{https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs}{Baptiste Auguié}.
#'
#' @seealso \url{https://rpkgs.datanovia.com/ggpubr/reference/ggarrange.html};
#' \url{https://wilkelab.org/cowplot/articles/shared_legends.html}
#'
#' @export
grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right"), legend_reference = 1) {

  plots <- list(...)
  position <- match.arg(position)

  # If want to find which panel has the majority then use code like below (or just make sure you have complete levels in data)
  # grab grobs from the data
  #ggplotGrob(temp2)$grobs -> temp4
  #grab the guidebox length of grobs
  #length(temp3[[which(sapply(temp3, function(x) x$name) == "guide-box")]][[1]][[1]][[1]])
  # Then compare all the grobs in list to find which is max... then pass this to the index in next section

  g <- ggplot2::ggplotGrob(plots[[legend_reference]] + ggplot2::theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + ggplot2::theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)

  combined <- switch(position,
                     "bottom" = gridExtra::arrangeGrob(do.call(arrangeGrob, gl),
                                                       legend,
                                                       ncol = 1,
                                                       heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight)),
                     "right" = gridExtra::arrangeGrob(do.call(arrangeGrob, gl),
                                                      legend,
                                                      ncol = 2,
                                                      widths = grid::unit.c(grid::unit(1, "npc") - lwidth, lwidth)))
  grid::grid.newpage()
  grid::grid.draw(combined)
  return(invisible(ggpubr::as_ggplot(combined))) # Swap to a ggplot grob from arrangeGrob...and invisible return
}



#' \code{ggproto} for \code{geom_bullseye}
#'
#' \code{StatBullseye} is the \code{ggproto} object required for \code{\link{geom_bullseye}}.
#'
#' @format NULL
#' @usage NULL
#' @keywords internal
#' @source Adapted from \href{https://stackoverflow.com/users/1457051/hrbrmstr}{Bob Rudis'} StackOverflow post:
#' \url{https://stackoverflow.com/questions/31635732/stacked-bubble-chart-bottom-aligned}
#' @export
StatBullseye <- ggplot2::ggproto("StatBullseye", ggplot2::Stat,
                                 required_aes = c("x", "group"),

                                 setup_params = function(data, params) {

                                   # Create a named vector for your values
                                   obs <- data$x
                                   obs <- setNames(obs, data$group)

                                   # Make obs into a list in reverse order to ensure overlap is correct
                                   obs <- as.list(rev(sort(obs)))

                                   # Pull out radii
                                   rads <- lapply(obs, "/", 2)

                                   # Grab max of all radii
                                   x <- max(sapply(rads, "["))

                                   # Pass the max as a global parameter to each group...
                                   params$circle_max <- x

                                   params
                                 },

                                 compute_group = function(data, scales, circle_max = .5) {

                                   #data <- data %>%
                                   #  dplyr::mutate(group = forcats::fct_rev(forcats::fct_reorder(group, x)))

                                   # Create a named vector for your values
                                   obs <- data$x
                                   obs <- setNames(obs, data$group)

                                   # Make obs into a list in reverse order to ensure overlap is correct
                                   obs <- as.list(rev(sort(obs)))

                                   # Pull out radii for each computed group
                                   rads <- lapply(obs, "/", 2)

                                   # build a data frame of created circles
                                   circle_data <- do.call(rbind.data.frame, lapply(1:length(rads), function(i) {
                                     create_circle(c(circle_max, rads[[i]]), rads[[i]])
                                   }))

                                   data.frame(x=circle_data$x, y=circle_data$y)
                                 }
)

#' Create bottom-aligned bullet chart
#'
#' \code{geom_bullseye} create a bullet chart with all the circles aligned to the bottom.
#'
#' Since this is a specialized plot, it requires data in a specific format (similar to line charts). The \code{x} parameter
#' must be the proportion of each category (i.e. the percentage one category is from the total). A \code{group} must also be
#' provided in a form of \code{fill, group, color}. It is also recommended to add \code{\link[ggplot2]{coord_equal}} and to adjust
#' the theme to something more minimal (e.g. \code{ggthemes::theme_map()}). Labeling the circles may be difficult at this time, but
#' a future version may make this easier without having to do additional calculations for ideal placement. Factor levels may
#' also need to be reordered (e.g. using \code{forcats}). Alternatively, instead of using this custom Stat, one could just use programming methods with ggplot2.
#'
#' @inheritParams ggplot2::geom_polygon
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(magrittr)
#' library(tibble)
#' library(forcats)
#'
#' # Create data
#' data <- tribble(~Grouping, ~Cases,
#'                 "Step1", 4000,
#'                 "Step2", 2300,
#'                 "ThirdStep", 200,
#'                 "Step4", 20)
#'
#' # Plot data
#' ggplot(data = data, aes(x = Cases, fill = fct_rev(fct_reorder(Grouping, Cases)))) +
#'   geom_bullseye()
#'   coord_equal() +
#'   ggthemes::theme_map() +
#'   theme(legend.title = element.blank(),
#'         legend.position = 'right')
#'  }
#'
#' @export
geom_bullseye <- function(mapping = NULL, data = NULL, geom = 'polygon',
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatBullseye, geom = geom, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

