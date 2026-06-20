\name{cpp_impute_mean}
\alias{cpp_impute_mean}
\alias{cpp_impute_median}
\alias{cpp_is_zero_variance}
\title{Rcpp-backed numeric helper functions}
\usage{
cpp_impute_mean(x)

cpp_impute_median(x)

cpp_is_zero_variance(x, tolerance = 1e-10)
}
\arguments{
\item{x}{A numeric vector.}
\item{tolerance}{Numeric threshold below which variance is treated as zero.}
}
\value{
\code{cpp_impute_mean()} and \code{cpp_impute_median()} return numeric vectors
with missing values replaced. \code{cpp_is_zero_variance()} returns \code{TRUE}
or \code{FALSE}.
}
\description{
Numeric helper functions backed by C++ through Rcpp in an installed package.
Each wrapper also has an equivalent R fallback for source-only development
sessions where native code has not been compiled yet.
}
\examples{
cpp_impute_mean(c(1, NA, 3))
cpp_impute_median(c(1, NA, 10))
cpp_is_zero_variance(c(5, 5, 5))
}
