#' Create a bubble plot of environmental indicators
#'
#' Generates a bubble plot comparing two selected variables with optional transformation.
#'
#' @param x Character string for x variable
#' @param y Character string for y variable
#' @param size Character string for bubble size variable (default population)
#' @param transform Transformation to apply: "none", "squared", "log", "reciprocal"
#' @param year_select Year to filter to. Default is 2019
#' @param data A cleaned data frame from clean_data(). If not provided,
#'   the function loads and cleans the data automatically.
#'
#' @return A ggplot object
#'
#' @importFrom dplyr mutate filter group_by summarise
#' @importFrom ggplot2 ggplot aes geom_point labs
#' @export

bubble <- function(x, y, size = "population", transform = "none",
                   year_select = 2019, data = NULL) {

  if (is.null(data)) data <- clean_data(load_data())

  plot_data <- data |>
    filter(year == year_select) |>
    group_by(country) |>
    summarise(
      grand_total              = sum(grand_total, na.rm = TRUE),
      gdp_per_capita           = mean(gdp_per_capita, na.rm = TRUE),
      plastic_waste_per_capita = mean(plastic_waste_per_capita, na.rm = TRUE),
      co2_per_capita           = mean(co2_per_capita, na.rm = TRUE),
      population               = mean(population, na.rm = TRUE),
      gdp_billions             = mean(gdp_billions, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      x_plot = apply_transform(.data[[x]], transform),
      y_plot = apply_transform(.data[[y]], transform)
    ) |>
    filter(
      !is.na(x_plot), !is.na(y_plot),
      !is.nan(x_plot), !is.nan(y_plot)
    )

  x_label <- if (transform == "none") x else paste0(transform, "(", x, ")")
  y_label <- if (transform == "none") y else paste0(transform, "(", y, ")")

  ggplot(plot_data, aes(
    x    = x_plot,
    y    = y_plot,
    size = .data[[size]]
  )) +
    geom_point(alpha = 0.65) +
    labs(
      x     = x_label,
      y     = y_label,
      title = "Bubble Plot of Country Indicators"
    )
}
