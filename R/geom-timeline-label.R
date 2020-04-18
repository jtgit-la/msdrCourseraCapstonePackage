#' @rdname geom_timeline
geom_timeline_label <- function(mapping = NULL, data = NULL,
                                position = "identity", show.legend = NA,
                                na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatTimelineLabel,
    geom = GeomTimelineLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname ggproto
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                            required_aes = c("x", "label", "n_max"),
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

                              print(coords)
                              sort(unique(coords$y))
                              y1_length <- diff(c(sort(unique(coords$y)),0.5)[1:2])/5
                              print(y1_length)

                              tl <- grid::segmentsGrob(x0 = coords$x,
                                                       x1 = coords$x,
                                                       y0 = coords$y,
                                                       y1 = coords$y + y1_length,
                                                       gp =
                                                         grid::gpar(
                                                           col = ggplot2::alpha("grey", coords$alpha))
                              )

                              tl_text <- grid::textGrob(
                                coords$x, coords$y + y1_length + 0.02,
                                label = coords$label,
                                just = c("left", "top"),
                                rot = 45,
                                gp = grid::gpar(fontsize = 8)
                              )

                              grid::gTree(children = grid::gList(tl, tl_text))

                            },

                            draw_key = ggplot2::draw_key_point
)
