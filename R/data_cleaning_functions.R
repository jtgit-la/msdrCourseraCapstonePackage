#'Clean/create \code{DATE} variable
#'
#'Create a DATE variable in a NOAA Significant Earthquakes dataset dataframe.
#'
#'@param df a dataframe from the NOAA Significant Earthquakes dataset
#'
#'@return A dataframe with new \code{DATE} variable
#'
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

#'Clean \code{LOCATION_NAME} variable
#'
#'Cleans the \code{LOCATION_NAME} variable in a NOAA Significant Earthquakes dataset dataframe.
#'
#'@param df a dataframe from the NOAA Significant Earthquakes dataset
#'
#'@return A dataframe with cleaned version of \code{LOCATION_NAME} variable
#'
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
