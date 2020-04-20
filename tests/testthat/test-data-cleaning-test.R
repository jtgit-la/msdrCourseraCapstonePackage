test_that("eq_clean_data works", {
  expect_is(eq_clean_data(read.delim(system.file("extdata", "signif.txt", package = "msdrCourseraCapstonePackage")))$DATE, "Date")
})

test_that("eq_location_clean works", {
  expect_is(eq_location_clean(read.delim(system.file("extdata", "signif.txt", package = "msdrCourseraCapstonePackage")))$LOCATION_NAME, "character")
})

test_that("eq_map works", {
  expect_is(eq_clean_data(read.delim(system.file("extdata", "signif.txt", package = "msdrCourseraCapstonePackage"))) %>%
              dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
              eq_map(annot_col = "DATE") , "leaflet")
})

test_that("geom_timeline works", {
  expect_is(ggplot(eq_data_plot, aes(x = DATE, y = COUNTRY, min_date = as.Date("1900-01-01"), max_date = as.Date("2000-01-01"), size = EQ_PRIMARY, color = TOTAL_DEATHS)) +
              geom_timeline(alpha = 0.3) +
              geom_timeline_label(aes(n_max = 3, label = LOCATION_NAME)) +
              theme_eq, "gg")
})
