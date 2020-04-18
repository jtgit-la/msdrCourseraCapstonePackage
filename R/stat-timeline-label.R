#' @rdname geom_timeline
stat_timeline_label <- function(mapping = NULL, data = NULL, geom = "timeline_label",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTLL,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname ggproto
StatTLL <- ggplot2::ggproto("StatTLL", ggplot2::Stat,
                            compute_group = function(data, scales){
                              date_min <- data$min_date[1]
                              date_max <- data$max_date[1]

                              data <- data %>%
                                dplyr::filter(x >= date_min, x <= date_max, !is.na(label))

                              n_max <- data$n_max[1]

                              if(!is.na(n_max)) {
                                data %>% dplyr::top_n(n_max, wt = size) %>% slice(c(1:n_max))
                              } else {
                                data
                              }


                            },
                            required_aes = c("x", "n_max")
)
