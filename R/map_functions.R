#'Map of NOAA Significant Earthquake data
#'
#'This function creates a leaflet map from a NOAA Significant Earthquake dataframe.
#'
#'@param data a dataframe from the NOAA Significant Earthquake dataframe.
#'
#'@param annot_col colummn name of the variable to be used for annotating the points.
#'
#'@return A leaflet map with the earthquake locations plotted as points, with radius proportional to the magnitude.
#'
#'@examples
#'
#'library(msdrCourseraCapstonePackage)
#'
#'data_path <- system.file("extdata", "signif.txt", package = "msdrCourseraCapstonePackage")
#'
#'eq_data_raw <- read.delim(data_path)
#'
#'eq_data <-
#'  eq_data_raw %>%
#'  eq_clean_data() %>%
#'  eq_location_clean()
#'
#'eq_data %>%
#'  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'  eq_map(annot_col = "DATE")
#'
#'readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
#'  eq_clean_data() %>%
#'  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'  dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'  eq_map(annot_col = "popup_text")
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

#' Creates label for eq_map()
#'
#' @rdname eq_map

eq_create_label <- function(data) {
  paste("<b>Location:</b>", data$LOCATION_NAME, "<br />",
        "<b>Magnitude:</b>", data$EQ_PRIMARY, "<br />",
        "<b>Total Deaths:</b>", data$TOTAL_DEATHS, "<br />")
}
