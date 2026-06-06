#' Load plastic waste and supplementary datasets
#'
#' Loads the full merged dataset from the package's inst/extdata folder.
#' The dataset joins plastic waste audit data with locally stored GDP,
#' population, and CO2 datasets.
#'
#' @return A data frame joining all four datasets with derived columns
#'   gdp_per_capita and plastic_waste_per_capita.
#'
#' @importFrom arrow read_parquet
#' @export

load_data <- function() {
  path <- system.file("extdata", "meta_set.parquet", package = "plasticRank")
  if (nchar(path) == 0) {
    stop("Could not locate 'meta_set.parquet' in the package. ",
         "Try reinstalling plasticRank.")
  }
  read_parquet(path)
}
