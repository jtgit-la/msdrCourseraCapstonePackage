
stat_timeline <- function(mapping = NULL, data = NULL, geom = "timeline",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTL,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



StatTL <- ggplot2::ggproto("StatTL", ggplot2::Stat,
                           compute_group = function(data, scales){
                             date_min <- data$min_date[1]
                             date_max <- data$max_date[1]

                             data %>% dplyr::filter(x >= date_min, x <= date_max)
                           },
                           required_aes = c("x", "min_date", "max_date")
)
