\name{replay_steps}
\alias{replay_steps}
\title{Apply transformation steps to a data frame}
\usage{
replay_steps(origin_df, steps = list())
}
\arguments{
\item{origin_df}{A non-empty data frame.}
\item{steps}{A list of step objects. Each step is a list containing at least an
\code{action} field and, for column-level steps, a \code{col} field.}
}
\value{
A transformed data frame.
}
\description{
Replays an ordered list of transformation steps against an original data frame.
The function is pure: it does not modify \code{origin_df}, which makes step
deletion safe because the current result can always be rebuilt from the
original data.
}
\examples{
df <- data.frame(x = c(1, NA, 3), label = c(" A ", "B ", " C"))
steps <- list(
  list(action = "impute", col = "x", method = "mean"),
  list(action = "trim_whitespace", col = "label")
)
replay_steps(df, steps)
}
