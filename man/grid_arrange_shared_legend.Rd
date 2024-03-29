% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{grid_arrange_shared_legend}
\alias{grid_arrange_shared_legend}
\title{Add a common legend with grid.arrange}
\source{
Adapted from \href{https://stackoverflow.com/users/4581430/steven-lockton}{Steven Lockton's}
\href{https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots}{StackOverflow contribution} and
\href{https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs}{Baptiste Auguié}.
}
\usage{
grid_arrange_shared_legend(
  ...,
  nrow = 1,
  ncol = length(list(...)),
  position = c("bottom", "right"),
  legend_reference = 1
)
}
\arguments{
\item{...}{Plot objects.}

\item{nrow}{Integer, number of rows for plotting objects.}

\item{ncol}{Integer, number of columns for plotting objects.}

\item{position}{Character vector passed to \code{theme} from \code{\link[ggplot2]{theme}}.}

\item{legend_reference}{An integer that references the plot, in the order provided, to determine where the shared legend
should originate from; this is defaulted to 1.}
}
\description{
Plot several separate plots into a combined figure with a shared legend using grobs and \code{\link[gridExtra]{grid.arrange}}.
}
\details{
\code{grid.arrange} can be more flexible for arranging grobs and plots of different types. If additional parameters are desired,
the function may need to be altered. For example, more legend positions could be added to the switch function.

Furthermore, the function assumes that every plot has the same amount of levels for hte factor. If some categories are missing from a plot,
ensure the correct plot reference is selected and when making each plot, ensure the legend includes categories that are not plotted. See the
example below for some more guidance.
}
\examples{
\dontrun{
library(ggplot2)
library(dplyr)
library(magrittr)

data1 <- iris \%>\% filter(Species == 'setosa') # Levels not dropped
data2 <- iris \%>\% filter(Species != 'setosa') # Levels not dropped
levels(data1$Species)
levels(data2$Species)

plot1 <- ggplot(data1, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
           geom_point() +
           scale_color_discrete(drop = FALSE) # Ensure that the legend shows all the categories for color!
plot2 <- ggplot(data2, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
           geom_point() +
           scale_color_discrete(drop = FALSE)

grid_arrange_shared_legend(plot1,plot2, legend_reference = 1) # Since both plots have the same legend, reference can be either 1 or 2
}
}
\seealso{
\url{https://rpkgs.datanovia.com/ggpubr/reference/ggarrange.html};
\url{https://wilkelab.org/cowplot/articles/shared_legends.html}
}
