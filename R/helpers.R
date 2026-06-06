# Internal helper functions for plasticRank
# These are not exported; they support the main package functions.

#' Return the vector of valid variable names
#'
#' Used by \code{rank_table()} and \code{all_ranks()} to validate user input.
#'
#' @return A character vector of valid variable names.
#' @keywords internal
#' @noRd
valid_variables <- function() {
  c(
    "co2_per_capita",
    "plastic_waste_per_capita",
    "gdp_per_capita",
    "population",
    "grand_total",
    "gdp_billions"
  )
}

#' Apply a named transformation to a numeric vector
#'
#' Supports \code{"none"}, \code{"squared"}, \code{"log"}, and
#' \code{"reciprocal"} transforms. Used by \code{bubble()}.
#'
#' @param v Numeric vector to transform.
#' @param transform Character string naming the transformation.
#'
#' @return Transformed numeric vector.
#' @keywords internal
#' @noRd
apply_transform <- function(v, transform) {
  switch(transform,
    squared    = v^2,
    log        = log(v),
    reciprocal = 1 / v,
    v  # default: "none" or unrecognised → identity
  )
}
