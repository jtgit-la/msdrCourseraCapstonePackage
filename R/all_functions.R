setwd("/home/joao/Desktop/msdrCourseraCapstonePackage/inst/extdata")

x <- read.delim("signif.txt")

library(tidyverse)

# objective of this function is to:
# After downloading and reading in the dataset, the overall task for this module is to write a
# function named eq_clean_data()that takes raw NOAA data frame and returns a clean data frame.
# The clean data frame should have the following:

# A date column created by uniting the year, month, day and converting it to the Date class
# LATITUDE and LONGITUDE columns converted to numeric class (actually when I read it in, the raw data appears to already have this, but have the conversion in code just in case)
#
# for now just replaced NA's with 01

eq_clean_data <- function(df) {
  df$MONTH[is.na(df$MONTH)] <- 1
  df$DAY[is.na(df$DAY)] <- 1

  df %>% dplyr::mutate(
        LATITUDE = as.numeric(LATITUDE),
        LONGITUDE = as.numeric(LONGITUDE),
        DATE = dplyr::if_else(
            YEAR < 0,
            lubridate::ymd(paste("0000", MONTH, DAY, sep = "-")) - lubridate::years(-YEAR),
            lubridate::ymd(paste(stringr::str_pad(YEAR, 4, pad = "0"), MONTH, DAY, sep = "-"))
          )
      )
}

xc <- eq_clean_data(x)

head(select(xc, YEAR, MONTH, DAY, DATE))




# In addition, write a function eq_location_clean() that cleans the LOCATION_NAME column by stripping out the country name
# (including the colon) and converts names to title case (as opposed to all caps).
# This will be needed later for annotating visualizations.
# This function should be applied to the raw data to produce a cleaned up version of the LOCATION_NAME column.

eq_location_clean <- function(df) {
  df %>%
    dplyr::mutate(
      LOCATION_NAME =
        LOCATION_NAME %>%
        stringr::str_extract("[^:]+$") %>%
        trimws() %>%
        tolower() %>%
        tools::toTitleCase()
    )
}

y <- eq_clean_data(x) %>% eq_location_clean()


view(data.frame(y$LOCATION_NAME, x$LOCATION_NAME))
# come back and make this function better. I.e, here are the ones that are not countries (did one pass through):
# c("", "Alabama", "Anatahan", "Atlantic", "Argentina", "Atlantic", "Balleny", "Bering", "Boso", "Boston", "British", "California", "Cano", "Canary", "Cascadia", "Central", "Comoros", "Costa", "Cote", "e", "East", "El", "Enshunada", "Flores", "French", "Futuna", "Gisborne", "Grfece", "Gulf", "Hawaiian", "Honshu", "Ibaraki", "Idaho", "Indian", "Iwate", "Kashima", "Kermadec", "Koori", "Kiribati", "Lake", "Lara", "Lhoknga", "Macquarie", "Marmara", "Mindanao", "Miyakojima", "Montana", "Mynamar", "n", "Ne", "Nemuro", "New", "North", "Northern", "North", "Nw", "Off", "s", "Sagami", "Saint", "Se", "Sea", "Sierra", "Sri", "Sw", "Syrian", "Texas", "The", "Tokaido", "Toyama", "UK", "Vanuata", "Virginia", "Virgin", "w", "Yellow", "Yatsushiro", "Washington")




# plot

StatTL <- ggplot2::ggproto("StatTL", ggplot2::Stat,
                           compute_group = function(data, scales){
                             date_min <- data$min_date[1]
                             date_max <- data$max_date[1]

                             data %>% dplyr::filter(x >= date_min, x <= date_max)
                           },
                           required_aes = c("x", "min_date", "max_date")
)


#'
#'Stat to filter eq data by dates
#'@param x The x aes. In this case this should be DATE
#'@param min_date The minimum date aes
#'@param max_date The maximum date aes
#'
#'

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


GeomTL <- ggplot2::ggproto("GeomTL", ggplot2::Geom,
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

#'
#'Geom to plot eq timeline
#'@param x the x aes
#'@param y the y aes (optional)
#'@param size the size aes (optional)
#'@param color the color aes (optional)
#'@examples
#'


geom_timeline <- function(mapping = NULL, data = NULL,
                          position = "identity", show.legend = NA,
                          na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatTL,
    geom = GeomTL,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#'
#'
#'ggplot2 theme for eq timeline plots
theme_eq <- ggplot2::theme_classic() +
  ggplot2::theme(axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(), legend.position = "bottom")


ggplot(filter(y, COUNTRY %in% c("USA", "CHINA"))) +
  geom_timeline(aes(x = DATE, y = COUNTRY, min_date = as.Date("1900-01-01"), max_date = as.Date("1910-01-01"), size = EQ_PRIMARY, color = TOTAL_DEATHS), alpha = 0.3) +
  labs(size = "Richter scale value") +
  labs(color = "# deaths") +
  theme_eq




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



GeomTLL <- ggplot2::ggproto("GeomTLL", ggplot2::Geom,
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


geom_timeline_label <- function(mapping = NULL, data = NULL,
                                position = "identity", show.legend = NA,
                                na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatTLL,
    geom = GeomTLL,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

data <- filter(y, COUNTRY %in% c("USA", "CHINA", "MEXICO", "CANADA"))

ggplot(data, aes(x = DATE, y = COUNTRY, min_date = as.Date("1900-01-01"), max_date = as.Date("1910-01-01"), size = EQ_PRIMARY, color = TOTAL_DEATHS)) +
  geom_timeline(alpha = 0.3) +
  labs(size = "Richter scale value") +
  geom_timeline_label(aes(label = LOCATION_NAME, n_max = 3)) +
  labs(color = "# deaths") +
  theme_eq





# Build a function called eq_map() that takes an argument data containing the filtered data frame
# with earthquakes to visualize. The function maps the epicenters (LATITUDE/LONGITUDE) and annotates
# each point with in pop up window containing annotation data stored in a column of the data frame.
# The user should be able to choose which column is used for the annotation in the pop-up with a function
# argument named annot_col. Each earthquake should be shown with a circle, and the radius of the circle
# should be proportional to the earthquake's magnitude (EQ_PRIMARY).
# Your code, assuming you have the earthquake data saved in your working directory as "earthquakes.tsv.gz",
# should be able to be used in the following way:
# readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
#  eq_clean_data() %>%
#  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#  eq_map(annot_col = "DATE")


#'Maps eq data
#'@param data the data set
#'@param annot_col variable name for popup text
#'@returns interactive map
#'@examples
#'eq_clean_data(eq_dataset_raw) %>%
#'    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'    eq_map(annot_col = "DATE")
#'
#'

eq_map <- function(data, annot_col = "") {
  if (annot_col != ""){
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(data = data, lng = ~ LONGITUDE, lat = ~ LATITUDE, radius = ~ EQ_PRIMARY, popup = ~ eval(parse(text = annot_col)), color = "#853aed", weight = 2)
  } else {
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(data = data, lng = ~ LONGITUDE, lat = ~ LATITUDE, radius = ~ EQ_PRIMARY)
  }
}

#'Creates label for eq_map
#'@param data the data
#'@examples
#'eq_clean_data(eq_dataset_raw) %>%
#'     dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'     dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'     eq_map(annot_col = "popup_text")
#'


eq_create_label <- function(data) {
  paste("<b>Location:</b>", data$LOCATION_NAME, "<br />",
        "<b>Magnitude:</b>", data$EQ_PRIMARY, "<br />",
        "<b>Total Deaths:</b>", data$TOTAL_DEATHS, "<br />")
}


library(leaflet)


eq_clean_data(y) %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    eq_map(annot_col = "DATE")




eq_clean_data(x) %>%
  eq_location_clean() %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")





