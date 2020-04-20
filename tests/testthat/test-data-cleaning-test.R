test_that("eq_clean_data works", {
  expect_is(eq_clean_data(read.delim(system.file("extdata", "signif.txt", package = "msdrCourseraCapstonePackage")))$DATE, "Date")
})
