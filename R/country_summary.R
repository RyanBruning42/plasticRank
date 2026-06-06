#' Summarize country-level indicators
#'
#' @param year_select Year to filter
#' @param data A cleaned data frame from clean_data(). If not provided,
#'   the function loads and cleans the data automatically.
#'
#' @return A summarized data frame
#' @export

country_summary <- function(year_select = 2019, data = NULL) {
  if (is.null(data)) data <- clean_data(load_data())

  data |>
    filter(year == year_select) |>
    group_by(country) |>
    summarise(
      gdp_per_capita           = mean(gdp_per_capita, na.rm = TRUE),
      co2_per_capita           = mean(co2_per_capita, na.rm = TRUE),
      plastic_waste_per_capita = mean(plastic_waste_per_capita, na.rm = TRUE),
      population               = mean(population, na.rm = TRUE),
      .groups = "drop"
    )
}
