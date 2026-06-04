test_that("rank_table() errors on invalid variable", {
  expect_snapshot(rank_table("invalid_var", 2019), error = TRUE)
})

test_that("rank_table() returns a data frame with correct columns", {
  skip_if_offline()
  skip_on_cran()

  result <- rank_table("gdp_per_capita", 2019)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("Country", "Value", "Rank"))
})

test_that("rank_table() ranks are positive integers in ascending order", {
  skip_if_offline()
  skip_on_cran()

  result <- rank_table("population", 2019)
  expect_true(all(result$Rank >= 1))
  expect_equal(result$Rank, sort(result$Rank))
})

test_that("rank_table() excludes Taiwan", {
  skip_if_offline()
  skip_on_cran()

  result <- rank_table("gdp_per_capita", 2019)
  expect_false("Taiwan" %in% result$Country)
})
