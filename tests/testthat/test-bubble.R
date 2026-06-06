make_mock_data <- function() {
  data.frame(
    country                  = c("Argentina", "Argentina", "Germany", "Germany"),
    year                     = c(2019, 2019, 2019, 2019),
    grand_total              = c(100, 200, 50, 75),
    gdp_per_capita           = c(10000, 10000, 45000, 45000),
    gdp_billions             = c(450, 450, 3800, 3800),
    plastic_waste_per_capita = c(0.002, 0.002, 0.001, 0.001),
    co2_per_capita           = c(4.5, 4.5, 8.5, 8.5),
    population               = c(45e6, 45e6, 83e6, 83e6),
    stringsAsFactors = FALSE
  )
}

test_that("bubble() returns a ggplot object", {
  result <- bubble("gdp_per_capita", "co2_per_capita", data = make_mock_data())
  expect_s3_class(result, "ggplot")
})

test_that("bubble() aggregates to one point per country", {
  result <- bubble("gdp_per_capita", "co2_per_capita", data = make_mock_data())
  expect_equal(nrow(result$data), 2)
})

test_that("bubble() axis labels are plain variable names with no transform", {
  result <- bubble("gdp_per_capita", "co2_per_capita", data = make_mock_data())
  expect_equal(result$labels$x, "gdp_per_capita")
  expect_equal(result$labels$y, "co2_per_capita")
})

test_that("bubble() axis labels include transform name when transform is applied", {
  result <- bubble("gdp_per_capita", "co2_per_capita",
                   transform = "log", data = make_mock_data())
  expect_equal(result$labels$x, "log(gdp_per_capita)")
  expect_equal(result$labels$y, "log(co2_per_capita)")
})

test_that("bubble() accepts all transform options without error", {
  for (tr in c("none", "squared", "log", "reciprocal")) {
    expect_s3_class(
      bubble("gdp_per_capita", "co2_per_capita",
             transform = tr, data = make_mock_data()),
      "ggplot"
    )
  }
})

test_that("bubble() filters to the correct year", {
  mixed <- rbind(make_mock_data(), transform(make_mock_data(), year = 2020))

  result_2019 <- bubble("gdp_per_capita", "co2_per_capita",
                         year_select = 2019, data = mixed)
  result_2020 <- bubble("gdp_per_capita", "co2_per_capita",
                         year_select = 2020, data = mixed)

  expect_equal(nrow(result_2019$data), 2)
  expect_equal(nrow(result_2020$data), 2)
})
