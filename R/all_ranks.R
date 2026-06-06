#' Bar chart of the top countries by a chosen variable
#'
#' Shows a horizontal bar chart of the highest-ranking countries for whatever
#' variable you pick. A quick way to see the leaders at a glance without
#' digging through a table.
#'
#' @param variable The column to rank by. Options are: co2_per_capita,
#'   plastic_waste_per_capita, gdp_per_capita, population, grand_total,
#'   or gdp_billions.
#' @param year_select The year to show data for. Either 2019 or 2020.
#'   Defaults to 2019.
#' @param top_n How many countries to include in the chart. Defaults to 15.
#' @param data A cleaned data frame from clean_data(). If not provided,
#'   the function loads and cleans the data automatically.
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' df_clean <- clean_data(load_data())
#' all_ranks("co2_per_capita", year_select = 2019, top_n = 10, data = df_clean)
#' }
#'
#' @importFrom dplyr slice_max
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal
#' @export

all_ranks <- function(variable, year_select = 2019, top_n = 15, data = NULL) {
  if (!variable %in% valid_variables()) {
    stop(
      "'", variable, "' is not a valid variable. Choose one of: ",
      paste(valid_variables(), collapse = ", ")
    )
  }

  if (is.null(data)) data <- clean_data(load_data())

  ranked <- rank_table(variable, year_select, data = data) |>
    slice_max(order_by = Value, n = top_n)

  ggplot(
    ranked,
    aes(x = Value, y = reorder(Country, Value))
  ) +
    geom_col() +
    labs(
      x     = variable,
      y     = "Country",
      title = paste0("Top ", top_n, " Countries by ", variable, " (", year_select, ")")
    ) +
    theme_minimal()
}
