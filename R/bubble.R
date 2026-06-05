#' Create a bubble plot of environmental indicators
#'
#' Generates a bubble plot comparing two selected variables with optional transformation.
#'
#'
#' @param x Character string for x variable
#' @param y Character string for y variable
#' @param size Character string for bubble size variable (default population)
#' @param transform Transformation to apply: "none", "squared", "log", "reciprocal"
#'
#' @return A ggplot object
#'
#' @importFrom dplyr mutate filter select group_by slice_max left_join
#' @importFrom ggplot2 ggplot aes geom_point labs
#' @export

bubble <- function(x, y, size = "population", transform = "none", year_select = 2019) {

  apply_transform <- function(v) {
    if (transform == "squared") return(v^2)
    if (transform == "log") return(log(v))
    if (transform == "reciprocal") return(1 / v)
    v
  }

  data <- load_data()

  plot_data <- data |>
    dplyr::filter(year == year_select) |>
    dplyr::filter(!is.na(country.x)) |>
    dplyr::group_by(country.x) |>
    dplyr::summarise(
      grand_total             = sum(grand_total, na.rm = TRUE),
      gdp_per_capita          = mean(gdp_per_capita, na.rm = TRUE),
      plastic_waste_per_capita = mean(plastic_waste_per_capita, na.rm = TRUE),
      co2_per_capita          = mean(co2_per_capita, na.rm = TRUE),
      population              = mean(population, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      x_plot = apply_transform(.data[[x]]),
      y_plot = apply_transform(.data[[y]])
    ) |>
    dplyr::filter(!is.na(x_plot), !is.na(y_plot), !is.nan(x_plot), !is.nan(y_plot))

  x_label <- if (transform == "none") x else paste0(transform, "(", x, ")")
  y_label <- if (transform == "none") y else paste0(transform, "(", y, ")")

  ggplot2::ggplot(plot_data, ggplot2::aes(
    x = x_plot,
    y = y_plot,
    size = .data[[size]]
  )) +
    ggplot2::geom_point(alpha = 0.65) +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      title = "Bubble Plot of Country Indicators"
    )
}
