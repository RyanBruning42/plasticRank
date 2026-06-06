#' Create a table ranking countries based on variable of interest
#'
#' Compute the numeric values, rank, needed to organize table
#'
#' @param variable Character string giving the variable of interest for ranking
#' @param year_select Year to filter to. Default is 2019
#' @param data A cleaned data frame from clean_data(). If not provided,
#'   the function loads and cleans the data automatically.
#'
#' @return A tibble containing country rankings with columns:
#' \describe{
#'   \item{Country}{Country name}
#'   \item{Value}{Mean value of the selected variable}
#'   \item{Rank}{Rank based on the selected variable}
#' }
#'
#' @importFrom dplyr filter mutate summarise arrange rename min_rank desc group_by
#' @importFrom DT datatable formatRound formatStyle styleColorBar
#'
#' @export

rank_table <- function(variable, year_select, data = NULL) {
  if (!variable %in% valid_variables()) {
    stop(
      "'", variable, "' is not a valid variable. Choose one of: ",
      paste(valid_variables(), collapse = ", ")
    )
  }

  if (is.null(data)) data <- clean_data(load_data())

  data |>
    filter(year == year_select) |>
    group_by(country) |>
    summarise(
      value = mean(.data[[variable]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(!is.na(value)) |>
    mutate(rank = min_rank(desc(value))) |>
    arrange(rank) |>
    rename(Country = country, Value = value, Rank = rank)
}
