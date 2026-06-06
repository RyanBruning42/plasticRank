test_that("load_data() returns a data frame", {
  result <- load_data()
  expect_s3_class(result, "data.frame")
})

test_that("load_data() returns expected columns", {
  result <- load_data()
  expected_cols <- c(
    "country", "year", "iso2", "grand_total",
    "gdp_billions", "population",
    "co2_per_capita", "gdp_per_capita",
    "plastic_waste_per_capita"
  )
  expect_true(all(expected_cols %in% names(result)))
})

test_that("load_data() returns rows for expected years", {
  result <- load_data()
  expect_true(all(c(2019, 2020) %in% unique(result$year)))
})
