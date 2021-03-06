% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_functions.R
\name{eq_map}
\alias{eq_map}
\alias{eq_create_label}
\title{Map of NOAA Significant Earthquake data}
\usage{
eq_map(data, annot_col = "")

eq_create_label(data)
}
\arguments{
\item{data}{a dataframe from the NOAA Significant Earthquake dataframe.}

\item{annot_col}{colummn name of the variable to be used for annotating the points.}
}
\value{
A leaflet map with the earthquake locations plotted as points, with radius proportional to the magnitude.
}
\description{
This function creates a leaflet map from a NOAA Significant Earthquake dataframe.
}
\examples{


\dontrun{library(msdrCourseraCapstonePackage)
library(magrittr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)

data_path <- system.file("extdata", "signif.txt", package = "msdrCourseraCapstonePackage")

eq_data_raw <- read.delim(data_path)

eq_data <-
 eq_data_raw \%>\%
 eq_clean_data() \%>\%
 eq_location_clean()

eq_data \%>\%
 dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) \%>\%
 eq_map(annot_col = "DATE")

eq_data \%>\%
 eq_clean_data() \%>\%
 dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) \%>\%
 dplyr::mutate(popup_text = eq_create_label(.)) \%>\%
 eq_map(annot_col = "popup_text")
}
}
