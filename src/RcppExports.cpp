#include <Rcpp.h>
#include <R_ext/Rdynload.h>

using namespace Rcpp;

NumericVector cpp_impute_mean_impl(NumericVector x);
NumericVector cpp_impute_median_impl(NumericVector x);
bool cpp_is_zero_variance_impl(NumericVector x, double tolerance);

RcppExport SEXP _autoMLR_cpp_impute_mean(SEXP xSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  rcpp_result_gen = Rcpp::wrap(cpp_impute_mean_impl(Rcpp::as<Rcpp::NumericVector>(xSEXP)));
  return rcpp_result_gen;
  END_RCPP
}

RcppExport SEXP _autoMLR_cpp_impute_median(SEXP xSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  rcpp_result_gen = Rcpp::wrap(cpp_impute_median_impl(Rcpp::as<Rcpp::NumericVector>(xSEXP)));
  return rcpp_result_gen;
  END_RCPP
}

RcppExport SEXP _autoMLR_cpp_is_zero_variance(SEXP xSEXP, SEXP toleranceSEXP) {
  BEGIN_RCPP
  Rcpp::RObject rcpp_result_gen;
  Rcpp::RNGScope rcpp_rngScope_gen;
  rcpp_result_gen = Rcpp::wrap(cpp_is_zero_variance_impl(
    Rcpp::as<Rcpp::NumericVector>(xSEXP),
    Rcpp::as<double>(toleranceSEXP)
  ));
  return rcpp_result_gen;
  END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
  {"_autoMLR_cpp_impute_mean", (DL_FUNC) &_autoMLR_cpp_impute_mean, 1},
  {"_autoMLR_cpp_impute_median", (DL_FUNC) &_autoMLR_cpp_impute_median, 1},
  {"_autoMLR_cpp_is_zero_variance", (DL_FUNC) &_autoMLR_cpp_is_zero_variance, 2},
  {NULL, NULL, 0}
};

RcppExport void R_init_autoMLR(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
