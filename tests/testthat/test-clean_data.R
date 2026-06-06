make_dirty_data <- function() {
  data.frame(
    country                  = c("Argentina", NA, "Taiwan Province of China", "Germany", "Brazil"),
    year                     = c(2019, 2019, 2019, 2019, 2019),
    grand_total              = c(100, 50, 200, 75, 80),
    gdp_per_capita           = c(10000, NA, 15000, 45000, NA),
    co2_per_capita           = c(4.5,   NA, 6.0,   8.5,   NA),
    plastic_waste_per_capita = c(0.002, NA, 0.003, 0.001, NA),
    stringsAsFactors = FALSE
  )
}

test_that("clean_data() returns a data frame", {
  result <- clean_data(make_dirty_data())
  expect_s3_class(result, "data.frame")
})

test_that("clean_data() removes rows with missing country", {
  result <- clean_data(make_dirty_data())
  expect_false(any(is.na(result$country)))
})

test_that("clean_data() removes Taiwan rows", {
  result <- clean_data(make_dirty_data())
  expect_false(any(grepl("Taiwan", result$country)))
})

test_that("clean_data() drops rows where all per-capita columns are NA", {
  result <- clean_data(make_dirty_data())
  all_na <- is.na(result$gdp_per_capita) &
            is.na(result$co2_per_capita) &
            is.na(result$plastic_waste_per_capita)
  expect_false(any(all_na))
})
