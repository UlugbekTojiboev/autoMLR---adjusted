#' Data-cleaning pipeline
#'
#' `Pipeline` is an R6 class that stores an original data frame and an ordered
#' list of transformation steps. Methods append validated steps and
#' [replay_steps()] applies those steps from the unchanged original data.
#'
#' @details
#' The class demonstrates object-oriented programming with R6 and defensive
#' programming through data-frame, column-name, and step validation.
#'
#' Public methods:
#'
#' * `$new(df = NULL)` creates a pipeline.
#' * `$add_step(step)` appends a transformation step.
#' * `$remove_step(idx)` removes a step by one-based index.
#' * `$impute_missing(col, method, fill_value)` adds an imputation step.
#' * `$encode_categoricals(col)` converts a column to factor.
#' * `$scale_features(col)` adds z-score scaling for a numeric column.
#' * `$apply_steps()` returns transformed data.
#' * `$zero_variance_cols()` returns numeric columns with near-zero variance.
#'
#' @examples
#' df <- data.frame(x = c(1, NA, 3), y = c("a", "b", "a"))
#' pipeline <- Pipeline$new(df)
#' pipeline$impute_missing("x", method = "mean")
#' pipeline$encode_categoricals("y")
#' pipeline$apply_steps()
#'
#' @export
Pipeline <- R6::R6Class(
  "Pipeline",
  public = list(
    origin_df = NULL,
    steps = NULL,

    initialize = function(df = NULL) {
      if (!is.null(df)) {
        private$validate_df(df)
        self$origin_df <- df
      }
      self$steps <- list()
    },

    add_step = function(step) {
      validate_pipeline_step(step)
      self$steps <- c(self$steps, list(step))
      invisible(self)
    },

    remove_step = function(idx) {
      if (!is.numeric(idx) || length(idx) != 1 || is.na(idx)) {
        stop("idx must be a single numeric step index.", call. = FALSE)
      }
      idx <- as.integer(idx)
      if (idx < 1 || idx > length(self$steps)) {
        stop("Step index out of range: ", idx, call. = FALSE)
      }
      self$steps <- self$steps[-idx]
      invisible(self)
    },

    impute_missing = function(col, method = "mean", fill_value = NULL) {
      private$validate_col_name(col)
      method <- match.arg(method, c("mean", "median", "constant", "drop"))
      self$add_step(list(
        action = "impute",
        col = col,
        method = method,
        fill_value = fill_value
      ))
      invisible(self)
    },

    encode_categoricals = function(col) {
      private$validate_col_name(col)
      self$add_step(list(action = "convert_type", col = col, to_type = "factor"))
      invisible(self)
    },

    scale_features = function(col) {
      private$validate_col_name(col)
      self$add_step(list(action = "scale_numeric", col = col))
      invisible(self)
    },

    apply_steps = function() {
      if (is.null(self$origin_df)) {
        stop("Pipeline has no data. Pass a data frame to Pipeline$new().", call. = FALSE)
      }
      replay_steps(self$origin_df, self$steps)
    },

    zero_variance_cols = function() {
      df <- self$apply_steps()
      num_cols <- names(which(vapply(df, is.numeric, logical(1))))
      Filter(function(col) isTRUE(cpp_is_zero_variance(df[[col]])), num_cols)
    },

    print = function(...) {
      cat("Pipeline [autoMLR]\n")
      cat("  Origin rows :", if (is.null(self$origin_df)) "none" else nrow(self$origin_df), "\n")
      cat("  Steps       :", length(self$steps), "\n")
      invisible(self)
    }
  ),
  private = list(
    validate_df = function(df) {
      if (!is.data.frame(df)) {
        stop("Pipeline expects a data.frame, got: ", class(df)[1], call. = FALSE)
      }
      if (nrow(df) == 0) {
        stop("Pipeline data.frame has zero rows.", call. = FALSE)
      }
      if (ncol(df) == 0) {
        stop("Pipeline data.frame has zero columns.", call. = FALSE)
      }
    },
    validate_col_name = function(col) {
      if (!is.character(col) || length(col) != 1 || is.na(col) || nchar(col) == 0) {
        stop("col must be a single non-empty character string.", call. = FALSE)
      }
    }
  )
)

#' Apply transformation steps to a data frame
#'
#' Replays an ordered list of transformation steps against an original data
#' frame. The function is pure: it does not modify `origin_df`, which makes step
#' deletion safe because the current result can always be rebuilt from the
#' original data.
#'
#' @param origin_df A non-empty data frame.
#' @param steps A list of step objects. Each step is a list containing at least
#'   an `action` field and, for column-level steps, a `col` field.
#'
#' @return A transformed data frame.
#' @export
#'
#' @examples
#' df <- data.frame(x = c(1, NA, 3), label = c(" A ", "B ", " C"))
#' steps <- list(
#'   list(action = "impute", col = "x", method = "mean"),
#'   list(action = "trim_whitespace", col = "label")
#' )
#' replay_steps(df, steps)
replay_steps <- function(origin_df, steps = list()) {
  validate_origin_df(origin_df)
  if (!is.list(steps)) {
    stop("steps must be a list.", call. = FALSE)
  }

  df <- origin_df
  if (length(steps) == 0) {
    return(df)
  }

  for (step in steps) {
    validate_pipeline_step(step)
    col <- step[["col"]]
    action <- step[["action"]]

    if (identical(action, "remove_duplicates")) {
      df <- df[!duplicated(df), , drop = FALSE]
      next
    }

    if (identical(action, "first_row_headers")) {
      if (nrow(df) < 1) {
        next
      }
      new_names <- as.character(unlist(df[1, ], use.names = FALSE))
      df <- df[-1, , drop = FALSE]
      colnames(df) <- make.names(new_names, unique = TRUE)
      rownames(df) <- NULL
      next
    }

    if (is.null(col) || length(col) != 1 || is.na(col) || !(col %in% colnames(df))) {
      next
    }

    if (identical(action, "convert_type")) {
      df[[col]] <- switch(
        step[["to_type"]],
        numeric = suppressWarnings(as.numeric(df[[col]])),
        character = as.character(df[[col]]),
        factor = as.factor(df[[col]]),
        date = suppressWarnings(as.Date(df[[col]])),
        binary = {
          raw <- tolower(trimws(as.character(df[[col]])))
          ifelse(raw %in% c("1", "yes", "true", "y", "t"), 1L, 0L)
        },
        df[[col]]
      )
    }

    if (identical(action, "round_values") && is.numeric(df[[col]])) {
      df[[col]] <- round(df[[col]], digits = step[["digits"]])
    }

    if (identical(action, "trim_whitespace")) {
      df[[col]] <- trimws(as.character(df[[col]]))
    }

    if (identical(action, "change_case")) {
      df[[col]] <- switch(
        step[["case"]],
        upper = toupper(as.character(df[[col]])),
        lower = tolower(as.character(df[[col]])),
        title = tools::toTitleCase(tolower(as.character(df[[col]]))),
        df[[col]]
      )
    }

    if (identical(action, "split_column")) {
      df[[col]] <- vapply(as.character(df[[col]]), function(x) {
        parts <- strsplit(x, step[["delim"]], fixed = TRUE)[[1]]
        if (length(parts) >= step[["part"]]) {
          trimws(parts[step[["part"]]])
        } else {
          NA_character_
        }
      }, character(1))
    }

    if (identical(action, "trim_chars")) {
      nchars <- step[["nchars"]]
      side <- step[["side"]]
      df[[col]] <- vapply(as.character(df[[col]]), function(x) {
        if (is.na(x)) {
          return(NA_character_)
        }
        if (identical(side, "left")) {
          substr(x, 1, nchars)
        } else {
          substr(x, max(1L, nchar(x) - nchars + 1L), nchar(x))
        }
      }, character(1))
    }

    if (identical(action, "scale_numeric") && is.numeric(df[[col]])) {
      mu <- mean(df[[col]], na.rm = TRUE)
      sigma <- stats::sd(df[[col]], na.rm = TRUE)
      if (!is.na(sigma) && sigma > 1e-10) {
        df[[col]] <- (df[[col]] - mu) / sigma
      }
    }

    if (identical(action, "impute")) {
      current_values <- df[[col]]
      method <- step[["method"]]
      if (identical(method, "drop")) {
        na_idx <- which(is.na(current_values) | as.character(current_values) == "")
        if (length(na_idx) > 0) {
          df <- df[-na_idx, , drop = FALSE]
        }
      } else if (identical(method, "mean") && is.numeric(current_values)) {
        df[[col]] <- cpp_impute_mean(current_values)
      } else if (identical(method, "median") && is.numeric(current_values)) {
        df[[col]] <- cpp_impute_median(current_values)
      } else if (identical(method, "constant")) {
        na_idx <- which(is.na(current_values) | as.character(current_values) == "")
        if (length(na_idx) > 0) {
          df[na_idx, col] <- step[["fill_value"]]
        }
      }
    }
  }

  df
}

#' Create a human-readable transformation label
#'
#' Converts one pipeline step into the one-line label shown in the Shiny
#' dashboard's applied-steps panel.
#'
#' @param step A pipeline step object.
#'
#' @return A character string describing the step.
#' @export
#'
#' @examples
#' step_label(list(action = "trim_whitespace", col = "name"))
step_label <- function(step) {
  validate_pipeline_step(step)
  switch(
    step[["action"]],
    remove_duplicates = "Remove Duplicate Rows",
    first_row_headers = "Use First Row as Headers",
    convert_type = paste0("Changed Type: ", step[["col"]], " -> ", step[["to_type"]]),
    round_values = paste0("Rounded: ", step[["col"]], " (", step[["digits"]], " dp)"),
    trim_whitespace = paste0("Trim Whitespace: ", step[["col"]]),
    change_case = paste0("Case: ", step[["col"]], " -> ", step[["case"]]),
    split_column = paste0(
      "Split: ", step[["col"]], " by '", step[["delim"]],
      "' part ", step[["part"]]
    ),
    trim_chars = paste0(
      "Trim Chars: ", step[["col"]], " -> ", step[["nchars"]],
      " (", step[["side"]], ")"
    ),
    impute = paste0(
      "Impute: ", step[["col"]], " -> ", step[["method"]],
      if (!is.null(step[["fill_value"]])) paste0(" ('", step[["fill_value"]], "')") else ""
    ),
    scale_numeric = paste0("Scale: ", step[["col"]], " using z-score"),
    step[["action"]]
  )
}

#' Render transformation settings as HTML
#'
#' Builds the small settings table used by the Shiny dashboard when a user opens
#' details for an applied transformation step.
#'
#' @param step A pipeline step object.
#'
#' @return An `htmltools` tag object containing an HTML table.
#' @export
step_settings_html <- function(step) {
  validate_pipeline_step(step)
  rows <- switch(
    step[["action"]],
    remove_duplicates = list(c("Operation", "Remove duplicate rows across all columns")),
    first_row_headers = list(c("Operation", "Promote first data row to column headers")),
    convert_type = list(c("Column", step[["col"]]), c("Convert to", step[["to_type"]])),
    round_values = list(c("Column", step[["col"]]), c("Decimal places", step[["digits"]])),
    trim_whitespace = list(c("Column", step[["col"]]), c("Operation", "trimws()")),
    change_case = list(c("Column", step[["col"]]), c("Case", step[["case"]])),
    split_column = list(
      c("Column", step[["col"]]),
      c("Delimiter", step[["delim"]]),
      c("Keep part", step[["part"]])
    ),
    trim_chars = list(
      c("Column", step[["col"]]),
      c("Max characters", step[["nchars"]]),
      c("Side", step[["side"]])
    ),
    impute = {
      base_rows <- list(c("Column", step[["col"]]), c("Method", step[["method"]]))
      if (!is.null(step[["fill_value"]])) {
        base_rows <- c(base_rows, list(c("Fill value", step[["fill_value"]])))
      }
      base_rows
    },
    scale_numeric = list(c("Column", step[["col"]]), c("Operation", "z-score scaling")),
    list(c("Action", step[["action"]]))
  )

  table_rows <- lapply(rows, function(row) {
    htmltools::tags$tr(
      htmltools::tags$td(htmltools::strong(row[[1]]), style = "width:140px;color:#475569;"),
      htmltools::tags$td(row[[2]], style = "color:#1e293b;")
    )
  })

  htmltools::tags$table(
    class = "table table-sm table-bordered",
    style = "font-size:.85rem;margin:0;",
    htmltools::tags$tbody(table_rows)
  )
}

validate_origin_df <- function(df) {
  if (!is.data.frame(df)) {
    stop("origin_df must be a data.frame.", call. = FALSE)
  }
  if (nrow(df) == 0) {
    stop("origin_df has zero rows.", call. = FALSE)
  }
  if (ncol(df) == 0) {
    stop("origin_df has zero columns.", call. = FALSE)
  }
  invisible(TRUE)
}

validate_pipeline_step <- function(step) {
  if (!is.list(step)) {
    stop("Each pipeline step must be a list.", call. = FALSE)
  }
  if (is.null(step[["action"]]) || !is.character(step[["action"]]) || length(step[["action"]]) != 1) {
    stop("Each pipeline step must include a single character action.", call. = FALSE)
  }
  invisible(TRUE)
}
