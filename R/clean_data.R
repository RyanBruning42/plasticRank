#' Clean the dataset
#'
#' Takes the raw data from load_data() and removes rows that would cause
#' problems in analysis. This filters out the large number of "EMPTY" entries
#' (audit records with no country attached), removes Taiwan rows that appear
#' as an artifact of the country code lookup, and drops rows where all three
#' per-capita columns are missing at the same time.
#'
#' @param data A data frame returned by load_data().
#'
#' @return A data frame with the problem rows removed. Reduces the dataset
#'   from roughly 60,000 rows down to about 12,000 usable country records.
#'
#' @examples
#' \dontrun{
#' df <- load_data()
#' df_clean <- clean_data(df)
#' }
#'
#' @importFrom dplyr filter
#' @export

clean_data <- function(data) {
  data |>
    filter(
      !is.na(country),
      country != "EMPTY",
      !grepl("Taiwan", country)
    ) |>
    filter(
      !(is.na(gdp_per_capita) &
        is.na(co2_per_capita) &
        is.na(plastic_waste_per_capita))
    )
}
