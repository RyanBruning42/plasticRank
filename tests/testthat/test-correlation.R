test_that("Class is correct",{
  class <- class(calc_correlation("co2_per_capita","gdp_per_capita"))

  expect_equal(class, "htest")
})

test_that("calc_correlation returns a numeric correlation estimate", {
  result <- calc_correlation("co2_per_capita", "gdp_per_capita")

  expect_true(is.numeric(result$estimate))
})

test_that("calc_correlation errors for invalid variables", {
  expect_error(
    calc_correlation("not_a_variable", "gdp_per_capita")
  )

  expect_error(
    calc_correlation("co2_per_capita", "not_a_variable")
  )
})
