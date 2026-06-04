make_mock_data <- function() {
  data.frame(
    country.x                = c("Argentina", "Argentina", "Germany", "Germany"),
    year                     = c(2019, 2019, 2019, 2019),
    grand_total              = c(100, 200, 50, 75),
    gdp_per_capita           = c(10000, 10000, 45000, 45000),
    plastic_waste_per_capita = c(0.002, 0.002, 0.001, 0.001),
    co2_per_capita           = c(4.5, 4.5, 8.5, 8.5),
    population               = c(45e6, 45e6, 83e6, 83e6),
    stringsAsFactors = FALSE
  )
}

test_that("bubble() returns a ggplot object", {
  result <- bubble(make_mock_data(), x = "gdp_per_capita", y = "co2_per_capita")
  expect_s3_class(result, "ggplot")
})

test_that("bubble() aggregates to one point per country", {
  result <- bubble(make_mock_data(), x = "gdp_per_capita", y = "co2_per_capita")
  expect_equal(nrow(result$data), 2)
})

test_that("bubble() axis labels are plain variable names with no transform", {
  result <- bubble(make_mock_data(), x = "gdp_per_capita", y = "co2_per_capita")
  expect_equal(result$labels$x, "gdp_per_capita")
  expect_equal(result$labels$y, "co2_per_capita")
})

test_that("bubble() axis labels include transform name when transform is applied", {
  result <- bubble(make_mock_data(), x = "gdp_per_capita", y = "co2_per_capita", transform = "log")
  expect_equal(result$labels$x, "log(gdp_per_capita)")
  expect_equal(result$labels$y, "log(co2_per_capita)")
})

test_that("bubble() accepts all transform options without error", {
  data <- make_mock_data()
  for (tr in c("none", "squared", "log", "reciprocal")) {
    expect_s3_class(
      bubble(data, x = "gdp_per_capita", y = "co2_per_capita", transform = tr),
      "ggplot"
    )
  }
})

test_that("bubble() filters to the correct year", {
  data <- make_mock_data()
  data_2020 <- data
  data_2020$year <- 2020
  mixed <- rbind(data, data_2020)

  result_2019 <- bubble(mixed, x = "gdp_per_capita", y = "co2_per_capita", year_select = 2019)
  result_2020 <- bubble(mixed, x = "gdp_per_capita", y = "co2_per_capita", year_select = 2020)

  expect_equal(nrow(result_2019$data), 2)
  expect_equal(nrow(result_2020$data), 2)
})
