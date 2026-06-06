#' Calculates the correlation coefficient between two variables
#'
#' Computes and returns a table with the pearson coefficient, as well as the
#' p-value, test statistic, and a confidence interval
#'
#' @param x The explanatory variable
#' @param y The response variable
#' @param data A cleaned data frame from clean_data(). If not provided,
#'   the function loads and cleans the data automatically.
#'
#' @return A htest object
#'
#' @export

calc_correlation <- function(x, y, data = NULL) {
  if (is.null(data)) data <- clean_data(load_data())

  cor.test(
    data[[x]],
    data[[y]],
    method = "pearson"
  )
}
