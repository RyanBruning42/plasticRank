test_that("load_data() returns a data frame", {
  skip_if_offline()
  skip_on_cran()

  result <- load_data()
  expect_s3_class(result, "data.frame")
})

test_that("load_data() returns expected columns", {
  skip_if_offline()
  skip_on_cran()

  result <- load_data()
  expected_cols <- c(
    "country", "iso2", "grand_total",
    "gdp_billions", "population",
    "co2_per_capita", "gdp_per_capita",
    "plastic_waste_per_capita"
  )
  expect_true(all(expected_cols %in% names(result)))
})
