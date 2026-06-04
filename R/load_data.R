#' Load plastic waste and supplementary datasets
#'
#' Downloads plastic waste audit data from the TidyTuesday repository and
#' joins it with locally stored GDP, population, and CO2 datasets. Country
#' ISO-2 codes are appended using \pkg{countrycode}.
#'
#' @return A data frame (\code{meta_set}) joining all four datasets with
#'   derived columns \code{gdp_per_capita} and \code{plastic_waste_per_capita}.
#'
#' @importFrom readr read_csv
#' @importFrom arrow read_parquet
#' @importFrom dplyr filter mutate rename
#' @importFrom countrycode countrycode
#' @export

load_data <- function() {

  plastics <- readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-01-26/plastics.csv",
    show_col_types = FALSE
  ) |>
    dplyr::filter(country != "EMPTY") |>
    dplyr::mutate(
      iso2 = countrycode::countrycode(country, origin = "country.name", destination = "iso2c")
    )

  gdp_data <- arrow::read_parquet(
    system.file("extdata", "gdp_data.parquet", package = "plasticRank")
  ) |>
    dplyr::rename("iso2" = "iso2c")

  population_data <- arrow::read_parquet(
    system.file("extdata", "population_data.parquet", package = "plasticRank")
  ) |>
    dplyr::rename("iso2" = "iso2c", "population" = "value")

  co2 <- arrow::read_parquet(
    system.file("extdata", "owid-co2-data.parquet", package = "plasticRank")
  ) |>
    dplyr::mutate(
      iso2 = countrycode::countrycode(iso_code, origin = "iso3c", destination = "iso2c")
    )

  meta_set <- plastics |>
    dplyr::left_join(gdp_data, by = c("iso2", "year" = "date")) |>
    dplyr::left_join(population_data, by = c("iso2", "year" = "date")) |>
    dplyr::left_join(co2, by = c("iso2", "year")) |>
    dplyr::rename("population" = "population.x") |>
    dplyr::mutate(
      gdp_per_capita = gdp_billions * 1e9 / population,
      plastic_waste_per_capita = grand_total / population
    )

  return(meta_set)
}
