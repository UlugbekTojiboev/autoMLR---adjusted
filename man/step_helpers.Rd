\name{step_label}
\alias{step_label}
\alias{step_settings_html}
\title{Pipeline step display helpers}
\usage{
step_label(step)

step_settings_html(step)
}
\arguments{
\item{step}{A pipeline step object.}
}
\value{
\code{step_label()} returns a character string. \code{step_settings_html()}
returns an \code{htmltools} tag object containing an HTML table.
}
\description{
Helpers used by the Shiny dashboard to render human-readable labels and
settings summaries for applied transformation steps.
}
\examples{
step_label(list(action = "trim_whitespace", col = "name"))
}
