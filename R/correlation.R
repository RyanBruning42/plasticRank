#' Calculates the correlation coefficient between two variables
#'
#' Computes and returns a table with the pearson coefficient, as well as the
#' p-value, test statistic, and a confidence interval
#'
#' @param explanatory The explanatory variable
#' @param response The response variable
#'
#' @return A htest object
#'
#' @export

calc_correlation <- function(x,y){

  data <- load_data()

  cor.test(
    data[[x]],
    data[[y]],
    method = "pearson"
  )
}
