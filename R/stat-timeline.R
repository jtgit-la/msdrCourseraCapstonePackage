#'@rdname geom_timeline

stat_timeline <- function(mapping = NULL, data = NULL, geom = "timeline",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTimeline,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @rdname msdrCourseraCapstonePackage-ggproto
StatTimeline <- ggplot2::ggproto("StatTimeline", ggplot2::Stat,
                           compute_group = function(data, scales){

                             date_min <- ifelse(is.null(data$min_date), min(data$x), data$min_date[1])
                             date_max <- ifelse(is.null(data$max_date), max(data$x), data$max_date[1])

                             data %>% dplyr::filter(x >= date_min, x <= date_max)
                           },
                           required_aes = c("x"),
                           non_missing_aes = c("min_date", "max_date")
)
