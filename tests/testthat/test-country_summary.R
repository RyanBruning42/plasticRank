make_mock_country_data <- function() {
  data.frame(
    country                  = c("Argentina", "Argentina", "Germany", "Germany"),
    year                     = c(2019, 2019, 2019, 2019),
    gdp_per_capita           = c(10000, 10500, 45000, 45500),
    co2_per_capita           = c(4.5, 4.3, 8.5, 8.7),
    plastic_waste_per_capita = c(0.002, 0.003, 0.001, 0.001),
    population               = c(45e6, 45e6, 83e6, 83e6),
    stringsAsFactors = FALSE
  )
}

test_that("country_summary() returns a data frame", {
  result <- country_summary(2019, data = make_mock_country_data())
  expect_s3_class(result, "data.frame")
})

test_that("country_summary() returns one row per country", {
  result <- country_summary(2019, data = make_mock_country_data())
  expect_equal(nrow(result), 2)
})

test_that("country_summary() returns expected columns", {
  result <- country_summary(2019, data = make_mock_country_data())
  expect_true(all(c("country", "gdp_per_capita", "co2_per_capita",
                    "plastic_waste_per_capita", "population") %in% names(result)))
})

test_that("country_summary() averages values correctly", {
  result <- country_summary(2019, data = make_mock_country_data())
  argentina <- result[result$country == "Argentina", ]
  expect_equal(argentina$gdp_per_capita, 10250)
})
