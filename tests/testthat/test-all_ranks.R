test_that("all_ranks() returns a ggplot object", {
  result <- all_ranks("co2_per_capita", year_select = 2019)
  expect_s3_class(result, "ggplot")
})

test_that("all_ranks() throws an error for an invalid variable", {
  expect_error(
    all_ranks("not_a_variable", 2019),
    "not a valid variable"
  )
})

test_that("all_ranks() respects top_n limit", {
  result <- all_ranks("co2_per_capita", year_select = 2019, top_n = 5)
  expect_lte(nrow(result$data), 5)
})
