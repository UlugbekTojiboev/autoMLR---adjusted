# autoMLR

`autoMLR` is an R package for interactive data cleaning. It wraps the original
Shiny dashboard project in a standard package structure and documents the main
functions with roxygen2 comments.

## Techniques included

- Advanced R functions with defensive validation
- Object-oriented programming through an R6 `Pipeline` class
- C++ integration through Rcpp helpers in `src/`
- Vectorised transformations for scaling, imputation, and string operations
- Shiny dashboard launched with `run_app()`
- Standard R package structure with `DESCRIPTION`, `NAMESPACE`, `R/`, `man/`,
  and roxygen2 documentation

## Install and run

```r
# From the repository root:
install.packages(c("bslib", "ggplot2", "htmltools", "R6", "Rcpp", "shiny", "shinyjs"))
devtools::install()

library(autoMLR)
run_app()
```

## Regenerate documentation

```r
install.packages("roxygen2")
roxygen2::roxygenise()
```

## Check the package

```r
devtools::check()
```

## Example pipeline usage

```r
library(autoMLR)

df <- data.frame(
  amount = c(10, NA, 30),
  label = c(" A ", "B ", " C")
)

pipeline <- Pipeline$new(df)
pipeline$impute_missing("amount", method = "mean")
pipeline$add_step(list(action = "trim_whitespace", col = "label"))

pipeline$apply_steps()
```