#'Geom for NOAA Earthquake dataset
#'
#'This geom is used to create timelines for the NOAA Significant Earthquakes dataset.
#'
#'@inheritParams ggplot2::layer
#'@export
#'
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
