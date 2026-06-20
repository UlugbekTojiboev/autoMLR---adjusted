\name{Pipeline}
\alias{Pipeline}
\title{Data-cleaning pipeline}
\description{
\code{Pipeline} is an R6 class that stores an original data frame and an ordered
list of transformation steps. Methods append validated steps and
\code{\link{replay_steps}} applies those steps from the unchanged original data.
}
\details{
The class demonstrates object-oriented programming with R6 and defensive
programming through data-frame, column-name, and step validation.

Public methods:
\itemize{
  \item \code{$new(df = NULL)} creates a pipeline.
  \item \code{$add_step(step)} appends a transformation step.
  \item \code{$remove_step(idx)} removes a step by one-based index.
  \item \code{$impute_missing(col, method, fill_value)} adds an imputation step.
  \item \code{$encode_categoricals(col)} converts a column to factor.
  \item \code{$scale_features(col)} adds z-score scaling for a numeric column.
  \item \code{$apply_steps()} returns transformed data.
  \item \code{$zero_variance_cols()} returns numeric columns with near-zero variance.
}
}
\examples{
df <- data.frame(x = c(1, NA, 3), y = c("a", "b", "a"))
pipeline <- Pipeline$new(df)
pipeline$impute_missing("x", method = "mean")
pipeline$encode_categoricals("y")
pipeline$apply_steps()
}
\keyword{classes}
