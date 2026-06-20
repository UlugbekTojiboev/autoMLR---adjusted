#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;

NumericVector cpp_impute_mean_impl(NumericVector x) {
  double sum = 0.0;
  int count = 0;

  for (int i = 0; i < x.size(); ++i) {
    if (!NumericVector::is_na(x[i])) {
      sum += x[i];
      count++;
    }
  }

  double replacement = count > 0 ? sum / count : 0.0;
  for (int i = 0; i < x.size(); ++i) {
    if (NumericVector::is_na(x[i])) {
      x[i] = replacement;
    }
  }

  return x;
}

NumericVector cpp_impute_median_impl(NumericVector x) {
  std::vector<double> values;
  values.reserve(x.size());

  for (int i = 0; i < x.size(); ++i) {
    if (!NumericVector::is_na(x[i])) {
      values.push_back(x[i]);
    }
  }

  double replacement = 0.0;
  if (!values.empty()) {
    size_t n = values.size();
    std::nth_element(values.begin(), values.begin() + n / 2, values.end());
    if (n % 2 != 0) {
      replacement = values[n / 2];
    } else {
      double upper = values[n / 2];
      std::nth_element(values.begin(), values.begin() + n / 2 - 1, values.end());
      replacement = (upper + values[n / 2 - 1]) / 2.0;
    }
  }

  for (int i = 0; i < x.size(); ++i) {
    if (NumericVector::is_na(x[i])) {
      x[i] = replacement;
    }
  }

  return x;
}

bool cpp_is_zero_variance_impl(NumericVector x, double tolerance = 1e-10) {
  double mean = 0.0;
  double m2 = 0.0;
  int count = 0;

  for (int i = 0; i < x.size(); ++i) {
    if (!NumericVector::is_na(x[i])) {
      count++;
      double delta = x[i] - mean;
      mean += delta / count;
      double delta2 = x[i] - mean;
      m2 += delta * delta2;
    }
  }

  if (count < 2) {
    return true;
  }

  double variance = m2 / (count - 1);
  return variance < tolerance;
}
