#' autoMLR: Interactive AutoML data cleaning
#'
#' The `autoMLR` package wraps the original Shiny data-cleaning project in a
#' reusable R package. It exposes the transformation pipeline as documented R
#' functions and an R6 class, provides Rcpp-backed imputation helpers, and
#' includes a small package-level Shiny launcher.
#'
#' @details
#' Main entry points:
#'
#' * [run_app()] launches the packaged Shiny dashboard.
#' * [Pipeline] stores raw data and ordered transformation steps.
#' * [replay_steps()] applies a step list to a data frame without side effects.
#'
#' @keywords internal
#' @useDynLib autoMLR, .registration = TRUE
#' @importFrom Rcpp evalCpp
"_PACKAGE"
