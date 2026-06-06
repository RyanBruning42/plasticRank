#' Summarize country-level indicators
#'
#' @param year_select Year to filter
#'
#' @return A summarized data frame
#' @export

country_summary <- function(year_select = 2019) {

  data <- load_data()

  data |>
    dplyr::filter(year == year_select, !is.na(country.x)) |>
    dplyr::group_by(country.x) |>
    dplyr::summarise(
      gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE),
      co2_per_capita = mean(co2_per_capita, na.rm = TRUE),
      plastic_waste_per_capita = mean(plastic_waste_per_capita, na.rm = TRUE),
      population = mean(population, na.rm = TRUE),
      .groups = "drop"
    )
}
