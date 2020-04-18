#'Geom for NOAA Earthquake dataset
#'
#'This geom is used to create timelines for the NOAA Significant Earthquakes dataset.
#'
#'@inheritParams ggplot2::layer
#'
#'@section Aesthetics:
#'
#' \code{geom_timeline()} understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \code{min_date}
#'   \item \code{max_date}
#'   \item \code{color}
#'   \item \code{size}
#'   \item \code{alpha}}
#'
#'@examples
#'library(tidyverse)
#'
#'data_path <- system.file("extdata", "signif.txt", package = "msdrCourseraCapstonePackage")
#'eq_data_raw <- read.delim(data_path)
#'eq_data <- eq_clean_data(eq_data_raw)
#'
#'eq_data_plot <- eq_data %>% filter(COUNTRY %in% c("CANADA", "USA", "MEXICO"))
#'
#'# basic timeline of entire dataset
#'ggplot(eq_data_plot, aes(x = DATE)) + geom_timeline() + theme_eq
#'
#'# filter timeline with a min date
#'ggplot(eq_data_plot, aes(x = DATE, min_date = as.Date("1900-01-01"))) + geom_timeline() + theme_eq
#'
#'# add a max date as well
#'ggplot(eq_data_plot, aes(x = DATE, min_date = as.Date("1900-01-01"), max_date = as.Date("1950-01-01"))) + geom_timeline() + theme_eq
#'
#'# use y aesthetic to add COUNTRY stratification
#'ggplot(eq_data_plot, aes(x = DATE, y = COUNTRY, min_date = as.Date("1900-01-01"), max_date = as.Date("1950-01-01"))) + geom_timeline() + theme_eq
#'
#'# use size and color of points to represent magnitude and number of casualties, and set alpha to 0.3
#'plot <- ggplot(eq_data_plot, aes(x = DATE, y = COUNTRY, min_date = as.Date("1900-01-01"), max_date = as.Date("1950-01-01"), size = EQ_PRIMARY, color = TOTAL_DEATHS)) + geom_timeline(alpha = 0.3) + theme_eq
geom_timeline <- function(mapping = NULL, data = NULL,
                          position = "identity", show.legend = NA,
                          na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatTimeline,
    geom = GeomTimeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



#'@rdname msdrCourseraCapstonePackage-ggproto
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                           required_aes = c("x"),
                           non_missing_aes = c("size", "shape", "colour"),
                           default_aes = ggplot2::aes(
                             y = 0,
                             shape = 19, colour = "grey", size = 1.5, fill = NA,
                             alpha = .75, stroke = 0.5
                           ),

                           draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                             if (is.character(data$shape)) {
                               data$shape <- translate_shape_string(data$shape)
                             }

                             coords <- coord$transform(data, panel_params)

                             tl <- grid::segmentsGrob(x0 = min(coords$x)-.01*diff(range(coords$x)),
                                                      x1 = max(coords$x) + .01*diff(range(coords$x)),
                                                      y0 = coords$y,
                                                      y1 = coords$y,
                                                      gp =
                                                        grid::gpar(
                                                          col = ggplot2::alpha("grey", coords$alpha))
                             )

                             tl_points <- grid::pointsGrob(
                               coords$x, coords$y,
                               pch = coords$shape,
                               gp = grid::gpar(
                                 col = ggplot2::alpha(coords$colour, coords$alpha),
                                 fill = ggplot2::alpha(coords$fill, coords$alpha),
                                 # Stroke is added around the outside of the point
                                 fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                 lwd = coords$stroke * .stroke / 2
                               )
                             )

                             grid::gTree(children = grid::gList(tl, tl_points))

                           },

                           draw_key = ggplot2::draw_key_point
)
