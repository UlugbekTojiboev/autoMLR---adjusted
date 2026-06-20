#' Impute missing numeric values with the mean
#'
#' Replaces `NA` values in a numeric vector with the mean of the non-missing
#' values. In an installed package this function calls the Rcpp implementation;
#' when native code is not compiled yet, it falls back to the same vectorised R
#' behaviour so examples and development workflows remain usable.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector with missing values replaced.
#' @export
#'
#' @examples
#' cpp_impute_mean(c(1, NA, 3))
cpp_impute_mean <- function(x) {
  x <- as.numeric(x)
  tryCatch(
    .Call("_autoMLR_cpp_impute_mean", x, PACKAGE = "autoMLR"),
    error = function(e) {
      replacement <- mean(x, na.rm = TRUE)
      if (is.nan(replacement)) {
        replacement <- 0
      }
      x[is.na(x)] <- replacement
      x
    }
  )
}

#' Impute missing numeric values with the median
#'
#' Replaces `NA` values in a numeric vector with the median of the non-missing
#' values. The installed package uses an Rcpp implementation; source-only
#' development sessions use an equivalent R fallback.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector with missing values replaced.
#' @export
#'
#' @examples
#' cpp_impute_median(c(1, NA, 10))
cpp_impute_median <- function(x) {
  x <- as.numeric(x)
  tryCatch(
    .Call("_autoMLR_cpp_impute_median", x, PACKAGE = "autoMLR"),
    error = function(e) {
      replacement <- stats::median(x, na.rm = TRUE)
      if (is.na(replacement)) {
        replacement <- 0
      }
      x[is.na(x)] <- replacement
      x
    }
  )
}

#' Detect zero-variance numeric vectors
#'
#' Checks whether a numeric vector has effectively zero variance after ignoring
#' missing values. This is used by [Pipeline] to identify columns that do not
#' carry modelling information.
#'
#' @param x A numeric vector.
#' @param tolerance Numeric threshold below which variance is treated as zero.
#'
#' @return `TRUE` when the vector has zero or near-zero variance, otherwise
#'   `FALSE`.
#' @export
#'
#' @examples
#' cpp_is_zero_variance(c(5, 5, 5))
#' cpp_is_zero_variance(c(1, 2, 3))
cpp_is_zero_variance <- function(x, tolerance = 1e-10) {
  x <- as.numeric(x)
  tryCatch(
    .Call("_autoMLR_cpp_is_zero_variance", x, as.numeric(tolerance), PACKAGE = "autoMLR"),
    error = function(e) {
      variance <- stats::var(x, na.rm = TRUE)
      isTRUE(!is.na(variance) && variance < tolerance)
    }
  )
}
