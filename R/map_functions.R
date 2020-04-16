

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

eq_create_label <- function(data) {
  paste("<b>Location:</b>", data$LOCATION_NAME, "<br />",
        "<b>Magnitude:</b>", data$EQ_PRIMARY, "<br />",
        "<b>Total Deaths:</b>", data$TOTAL_DEATHS, "<br />")
}
