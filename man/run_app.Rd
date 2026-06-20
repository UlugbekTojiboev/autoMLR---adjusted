\name{run_app}
\alias{run_app}
\alias{automlr_ui}
\alias{automlr_server}
\title{Launch the autoMLR Shiny app}
\usage{
run_app(...)

automlr_ui()

automlr_server(input, output, session)
}
\arguments{
\item{...}{Additional arguments passed to \code{shiny::runApp()},
such as \code{launch.browser = TRUE}.}
\item{input}{Shiny input object.}
\item{output}{Shiny output object.}
\item{session}{Shiny session object.}
}
\value{
\code{run_app()} is called for its side effect of launching a Shiny app.
\code{automlr_ui()} returns a Shiny UI object. \code{automlr_server()} returns
\code{NULL} invisibly.
}
\description{
Starts a packaged Shiny dashboard for uploading a CSV file, applying common
data-cleaning transformations, previewing the cleaned data, and viewing a
simple distribution plot for the selected column.
}
\examples{
if (interactive()) {
  run_app()
}
}
