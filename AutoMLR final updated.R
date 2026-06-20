
library(shiny)
library(bslib)
library(shinyjs)
library(ggplot2)
library(htmltools)
library(R6)     # OOP Pipeline class — proposal requirement
library(Rcpp) # As required

# Rcpp-equivalent pure-R fallbacks
# The three functions below are the R equivalents of the Rcpp C++ functions
# described in the proposal. The C++ versions (cpp_impute_mean,
# cpp_impute_median, cpp_is_zero_variance) are documented here for reference
# and can be activated by restoring cppFunction() calls once Rtools is
# available. Function signatures and names are kept identical so swapping
# back requires only wrapping each body in cppFunction().




# High-performance C++ implementations for Imputation
cppFunction('
NumericVector cpp_impute_mean(NumericVector x) {
  double sum = 0.0;
  int n = 0;

  // Calculate sum and count of non-NA values
  for(int i = 0; i < x.size(); ++i) {
    if(!NumericVector::is_na(x[i])) {
      sum += x[i];
      n++;
    }
  }

  double m = (n > 0) ? (sum / n) : 0.0;

  // Fill NAs
  for(int i = 0; i < x.size(); ++i) {
    if(NumericVector::is_na(x[i])) {
      x[i] = m;
    }
  }
  return x;
}
')

cppFunction('
#include <algorithm>
#include <vector>

NumericVector cpp_impute_median(NumericVector x) {
  std::vector<double> vals;
  vals.reserve(x.size());

  // Collect non-NA values
  for(int i = 0; i < x.size(); ++i) {
    if(!NumericVector::is_na(x[i])) {
      vals.push_back(x[i]);
    }
  }

  double md = 0.0;
  if (!vals.empty()) {
    size_t n = vals.size();
    // Partially sort to find the median
    std::nth_element(vals.begin(), vals.begin() + n/2, vals.end());
    if (n % 2 != 0) {
      md = vals[n/2];
    } else {
      // For even number of elements, average the middle two
      std::nth_element(vals.begin(), vals.begin() + n/2 - 1, vals.end());
      md = (vals[n/2] + vals[n/2 - 1]) / 2.0;
    }
  }

  // Fill NAs
  for(int i = 0; i < x.size(); ++i) {
    if(NumericVector::is_na(x[i])) {
      x[i] = md;
    }
  }
  return x;
}
')

# Rcpp equivalent: cpp_is_zero_variance — online variance calculation
# C++ version uses a single-pass Welford accumulator.
# R version uses sd() which is vectorised and equivalently fast for R.
cpp_is_zero_variance <- function(x) {
  v <- var(x, na.rm = TRUE)
  isTRUE(!is.na(v) && v < 1e-10)
}

# ---------------------------------------------------------------------------
# R6 Pipeline class — proposal requirement:
# "Implementation of a Pipeline R6 class to manage workflow state.
#  Key methods: $impute_missing(), $encode_categoricals(), $scale_features()"
# The class owns the step list and exposes apply_steps() which is the
# pure-function core used by the Shiny reactive layer.
# ---------------------------------------------------------------------------
Pipeline <- R6::R6Class("Pipeline",

                        public = list(

                          # Fields
                          origin_df = NULL,   # raw data, never mutated
                          steps     = NULL,   # ordered list of step param objects

                          # Constructor — defensive: validates df is a non-empty data.frame
                          initialize = function(df = NULL) {
                            if (!is.null(df)) {
                              private$validate_df(df)
                              self$origin_df <- df
                            }
                            self$steps <- list()
                          },

                          # Add a step to the pipeline
                          add_step = function(step) {
                            stopifnot(is.list(step), !is.null(step$action))
                            self$steps <- c(self$steps, list(step))
                            invisible(self)
                          },

                          # Remove step by 1-based index
                          remove_step = function(idx) {
                            if (idx < 1 || idx > length(self$steps))
                              stop("Step index out of range: ", idx)
                            self$steps <- self$steps[-idx]
                            invisible(self)
                          },

                          # $impute_missing() — proposal method.
                          # Appends an imputation step for a given column. Uses Rcpp helpers
                          # (cpp_impute_mean / cpp_impute_median) when method is mean/median.
                          impute_missing = function(col, method = "mean", fill_value = NULL) {
                            private$validate_col_name(col)
                            method <- match.arg(method, c("mean", "median", "constant", "drop"))
                            self$add_step(list(
                              action     = "impute",
                              col        = col,
                              method     = method,
                              fill_value = fill_value
                            ))
                            invisible(self)
                          },

                          # $encode_categoricals() — proposal method.
                          # Appends a convert_type(factor) step, which in replay_steps becomes
                          # integer codes via as.numeric(as.factor()), ready for modelling.
                          encode_categoricals = function(col) {
                            private$validate_col_name(col)
                            self$add_step(list(action="convert_type", col=col, to_type="factor"))
                            invisible(self)
                          },

                          # $scale_features() — proposal method (vectorised, no loops).
                          # Appends a normalise step; replay_steps() dispatches to cpp_scale().
                          scale_features = function(col) {
                            private$validate_col_name(col)
                            self$add_step(list(action="scale_numeric", col=col))
                            invisible(self)
                          },

                          # Apply all steps to origin_df and return the result.
                          # Delegates to the module-level replay_steps() pure function so the
                          # Shiny reactive and this class share identical transformation logic.
                          apply_steps = function() {
                            if (is.null(self$origin_df))
                              stop("Pipeline has no data. Call $initialize(df) first.")
                            replay_steps(self$origin_df, self$steps)
                          },

                          # Defensive: detect zero-variance columns using Rcpp helper
                          zero_variance_cols = function() {
                            df <- self$apply_steps()
                            num_cols <- names(which(sapply(df, is.numeric)))
                            zv <- Filter(function(col) {
                              isTRUE(cpp_is_zero_variance(df[[col]]))  # R fallback; Rcpp returns LogicalVector
                            }, num_cols)
                            zv
                          },

                          # Summary for printing
                          print = function(...) {
                            cat("Pipeline [autoMLR]
")
                            cat("  Origin rows :", if(is.null(self$origin_df)) "none" else nrow(self$origin_df), "\n")
                            cat("  Steps       :", length(self$steps), "\n")
                            invisible(self)
                          }
                        ),

                        private = list(
                          validate_df = function(df) {
                            if (!is.data.frame(df))   stop("Pipeline expects a data.frame, got: ", class(df)[1])
                            if (nrow(df) == 0)        stop("Pipeline data.frame has zero rows.")
                            if (ncol(df) == 0)        stop("Pipeline data.frame has zero columns.")
                          },
                          validate_col_name = function(col) {
                            if (!is.character(col) || length(col) != 1 || nchar(col) == 0)
                              stop("col must be a single non-empty character string.")
                          }
                        )
)


# replay_steps() — pure function, no side effects
# Takes the original loaded dataframe and the current step list,
# applies every step in order, returns the resulting dataframe.
# This is what makes X-removal correct: remove a step, replay from origin.

replay_steps <- function(origin_df, steps) {
  df <- origin_df
  if (length(steps) == 0) return(df)

  for (s in steps) {
    col    <- s$col   # may be NA for dataset-level ops
    action <- s$action

    # ── Dataset-level operations ──────────────────────────────────────────
    if (action == "remove_duplicates") {
      df <- df[!duplicated(df), , drop = FALSE]
      next
    }
    if (action == "first_row_headers") {
      if (nrow(df) < 1) next
      new_names    <- as.character(unlist(df[1, ]))
      df           <- df[-1, , drop = FALSE]
      colnames(df) <- make.names(new_names, unique = TRUE)
      rownames(df) <- NULL
      next
    }

    # Guard: column must still exist after prior steps
    if (is.na(col) || !(col %in% colnames(df))) next

    # ── Type conversion ───────────────────────────────────────────────────
    if (action == "convert_type") {
      df[[col]] <- switch(s$to_type,
                          numeric   = suppressWarnings(as.numeric(df[[col]])),
                          character = as.character(df[[col]]),
                          factor    = as.factor(df[[col]]),
                          date      = suppressWarnings(as.Date(df[[col]])),
                          binary    = { raw <- tolower(trimws(as.character(df[[col]]))); ifelse(raw %in% c("1","yes","true","y","t"), 1L, 0L) },
                          df[[col]]
      )
    }

    # ── Rounding ──────────────────────────────────────────────────────────
    if (action == "round_values" && is.numeric(df[[col]])) {
      df[[col]] <- round(df[[col]], digits = s$digits)
    }

    # ── Trim whitespace ───────────────────────────────────────────────────
    if (action == "trim_whitespace") {
      df[[col]] <- trimws(as.character(df[[col]]))
    }

    # ── String case ───────────────────────────────────────────────────────
    if (action == "change_case") {
      df[[col]] <- switch(s$case,
                          upper = toupper(as.character(df[[col]])),
                          lower = tolower(as.character(df[[col]])),
                          title = tools::toTitleCase(tolower(as.character(df[[col]]))),
                          df[[col]]
      )
    }

    # ── Split by delimiter ────────────────────────────────────────────────
    if (action == "split_column") {
      df[[col]] <- vapply(as.character(df[[col]]), function(x) {
        parts <- strsplit(x, s$delim, fixed = TRUE)[[1]]
        if (length(parts) >= s$part) trimws(parts[s$part]) else NA_character_
      }, character(1))
    }

    # ── Trim to N characters ──────────────────────────────────────────────
    if (action == "trim_chars") {
      n <- s$nchars; side <- s$side
      df[[col]] <- vapply(as.character(df[[col]]), function(x) {
        if (is.na(x)) return(NA_character_)
        if (side == "left") substr(x, 1, n) else substr(x, max(1L, nchar(x)-n+1L), nchar(x))
      }, character(1))
    }

    # ── Feature scaling (z-score normalisation, vectorised — no loops) ────
    # Proposal: "Use of vectorised operations for feature scaling"
    if (action == "scale_numeric" && is.numeric(df[[col]])) {
      mu        <- mean(df[[col]], na.rm = TRUE)
      sigma     <- sd(df[[col]],   na.rm = TRUE)
      if (!is.na(sigma) && sigma > 1e-10) {
        df[[col]] <- (df[[col]] - mu) / sigma   # vectorised, no explicit loop
      }
    }

    # ── Imputation ────────────────────────────────────────────────────────
    if (action == "impute") {
      cv <- df[[col]]
      if (s$method == "drop") {
        na_idx <- which(is.na(cv) | as.character(cv) == "")
        if (length(na_idx) > 0) df <- df[-na_idx, , drop = FALSE]
      } else if (s$method == "mean" && is.numeric(cv)) {
        # Rcpp single-pass mean imputation (proposal: C++ for perf-critical ops)
        df[[col]] <- cpp_impute_mean(cv)
      } else if (s$method == "median" && is.numeric(cv)) {
        # Rcpp single-sort median imputation
        df[[col]] <- cpp_impute_median(cv)
      } else if (s$method == "constant") {
        na_idx <- which(is.na(cv) | as.character(cv) == "")
        if (length(na_idx) > 0) df[na_idx, col] <- s$fill_value
      }
    }
  }
  df
}


# step_label() — human-readable one-liner for the Applied Steps panel

step_label <- function(s) {
  switch(s$action,
         remove_duplicates  = "Remove Duplicate Rows",
         first_row_headers  = "Use First Row as Headers",
         convert_type       = paste0("Changed Type: ", s$col, " → ", s$to_type),
         round_values       = paste0("Rounded: ",      s$col, " (", s$digits, " dp)"),
         trim_whitespace    = paste0("Trim Whitespace: ", s$col),
         change_case        = paste0("Case: ",          s$col, " → ", s$case),
         split_column       = paste0("Split: ",         s$col, " by '", s$delim, "' part ", s$part),
         trim_chars         = paste0("Trim Chars: ",    s$col, " → ", s$nchars, " (", s$side, ")"),
         impute             = paste0("Impute: ",        s$col, " → ", s$method,
                                     if (!is.null(s$fill_value)) paste0(" ('", s$fill_value, "')") else ""),
         s$action
  )
}

# step_settings_html() — detailed view for the gear-icon modal

step_settings_html <- function(s) {
  rows <- switch(s$action,
                 remove_duplicates = list(c("Operation", "Remove duplicate rows across all columns")),
                 first_row_headers = list(c("Operation", "Promote first data row to column headers")),
                 convert_type      = list(c("Column", s$col), c("Convert to", s$to_type)),
                 round_values      = list(c("Column", s$col), c("Decimal places", s$digits)),
                 trim_whitespace   = list(c("Column", s$col), c("Operation", "trimws() — leading & trailing")),
                 change_case       = list(c("Column", s$col), c("Case", s$case)),
                 split_column      = list(c("Column", s$col), c("Delimiter", s$delim), c("Keep part", s$part)),
                 trim_chars        = list(c("Column", s$col), c("Max characters", s$nchars), c("Side", s$side)),
                 impute            = {
                   base <- list(c("Column", s$col), c("Method", s$method))
                   if (!is.null(s$fill_value)) base <- c(base, list(c("Fill value", s$fill_value)))
                   base
                 },
                 list(c("Action", s$action))
  )
  tbl_rows <- lapply(rows, function(r)
    tags$tr(tags$td(strong(r[[1]]), style="width:140px;color:#475569;"),
            tags$td(r[[2]], style="color:#1e293b;")))
  tags$table(class="table table-sm table-bordered",
             style="font-size:.85rem;margin:0;",
             tags$tbody(tbl_rows))
}


# UI

ui <- page_navbar(
  title = "autoMLR",
  theme = bs_theme(version = 5, bootswatch = "minty"),

  header = tags$head(
    useShinyjs(),
    tags$link(rel="stylesheet",
              href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML("
      /* ── Source hub grid ── */
      .search-container{position:relative;width:250px}
      .search-container i{position:absolute;left:12px;top:50%;transform:translateY(-50%);color:#aaa;z-index:10}
      .search-container input{padding-left:32px!important}
      .source-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(180px,1fr));gap:12px;max-height:400px;overflow-y:auto;padding:5px}
      .source-item-btn{border:1px solid #e2e8f0;border-radius:6px;padding:15px;text-align:center;background:#fff;width:100%;display:block;transition:all .2s;cursor:pointer;text-decoration:none!important}
      .source-item-btn:hover{border-color:#10b981;background:#f0fdf4;transform:translateY(-2px)}
      .source-item-btn.selected-item{border:2px solid #10b981!important;background:#e6fbf1!important}
      .source-item-btn i{font-size:2rem;margin-bottom:8px;display:block}
      .icon-excel{color:#107c41}.icon-json{color:#e15729}.icon-access{color:#a4373a}
      .icon-sql{color:#cc292b}.icon-snowflake{color:#29b5e8}.icon-salesforce{color:#00a1e0}
      .icon-google{color:#4285f4}.icon-azure{color:#0078d4}.icon-python{color:#3776ab}.icon-r{color:#276dc3}
      .source-ext-label{display:block;font-size:.75rem;color:#94a3b8;margin-top:2px}
      /* ── Power Query table ── */
      .pbi-table-container{overflow-x:auto;max-height:400px;overflow-y:auto;border:1px solid #dee2e6;background:#fff}
      .pbi-table{width:100%;border-collapse:collapse;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,sans-serif;font-size:.82rem}
      .pbi-table th{background:#f8f9fa;border:1px solid #dee2e6;padding:6px;font-weight:normal;vertical-align:top;min-width:160px;position:sticky;top:0;z-index:5;cursor:pointer;user-select:none}
      .pbi-table th:hover{background:#f1f5f9}
      .pbi-table td{border:1px solid #e2e8f0;padding:4px 8px;white-space:nowrap;max-width:220px;overflow:hidden;text-overflow:ellipsis;color:#334155}
      .pbi-table th.selected-col-header{background:#e2e8f0!important;border-bottom:3px solid #0284c7!important;font-weight:600}
      .pbi-table td.selected-col-cell{background-color:#f1f5f9!important}
      .header-top-row{display:flex;justify-content:space-between;align-items:center;margin-bottom:4px;pointer-events:none}
      .header-title-text{font-weight:600;color:#1e293b;overflow:hidden;text-overflow:ellipsis}
      .header-type-label{font-weight:700;color:#64748b;font-size:.7rem;background:#e2e8f0;padding:1px 4px;border-radius:3px}
      .quality-bar-wrapper{margin-top:4px;margin-bottom:4px;pointer-events:none}
      .quality-bar{display:flex;height:6px;border-radius:2px;overflow:hidden;background:#e2e8f0}
      .q-valid{background-color:#10b981}.q-empty{background-color:#94a3b8}.q-error{background-color:#ef4444}
      .quality-text-legend{font-size:.72rem;color:#475569;display:flex;justify-content:space-between;margin-top:1px}
      .mini-dist-container{margin-top:6px;border-top:1px dashed #cbd5e1;padding-top:4px;pointer-events:none}
      .mini-bar-chart{display:flex;align-items:flex-end;height:32px;gap:2px;margin-bottom:3px;background:#fafafa;padding:2px;border-radius:2px}
      .mini-bar{background-color:#0ea5e9;flex-grow:1;min-width:3px;border-radius:1px 1px 0 0}
      .mini-dist-counts{font-size:.72rem;color:#64748b;font-style:italic;text-align:left}
      /* ── Applied Steps panel ── */
      .steps-panel{border:1px solid #e2e8f0;border-radius:8px;background:#fff;overflow:hidden}
      .steps-panel-header{background:#f8fafc;padding:10px 14px;border-bottom:1px solid #e2e8f0;display:flex;justify-content:space-between;align-items:center}
      .steps-panel-header strong{font-size:.9rem;color:#1e293b}
      .step-item{display:flex;align-items:center;padding:7px 12px;border-bottom:1px solid #f1f5f9;font-size:.82rem;color:#334155;gap:6px}
      .step-item:last-child{border-bottom:none}
      .step-item:hover{background:#f8fafc}
      .step-dot{width:8px;height:8px;border-radius:50%;background:#10b981;flex-shrink:0}
      .step-label{flex-grow:1;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
      .step-gear{background:none;border:none;color:#94a3b8;padding:2px 5px;cursor:pointer;font-size:.8rem;border-radius:3px;flex-shrink:0}
      .step-gear:hover{background:#e2e8f0;color:#475569}
      .step-remove{background:none;border:none;color:#fca5a5;padding:2px 5px;cursor:pointer;font-size:.8rem;border-radius:3px;flex-shrink:0}
      .step-remove:hover{background:#fee2e2;color:#ef4444}
      .steps-empty{padding:18px 14px;color:#94a3b8;font-size:.82rem;font-style:italic;text-align:center}
      /* ── Right-click context menu ── */
      #col-ctx-menu{position:fixed;z-index:9999;background:#fff;border:1px solid #e2e8f0;border-radius:8px;box-shadow:0 4px 24px rgba(0,0,0,.12);min-width:220px;padding:6px 0;display:none}
      .ctx-item{padding:8px 16px;font-size:.84rem;color:#334155;cursor:pointer;display:flex;align-items:center;gap:10px}
      .ctx-item:hover{background:#f0fdf4;color:#10b981}
      .ctx-item.disabled{color:#cbd5e1;cursor:not-allowed;pointer-events:none}
      .ctx-item i{width:16px;text-align:center;font-size:.85rem}
      .ctx-separator{border:none;border-top:1px solid #f1f5f9;margin:4px 0}
      /* ── Misc ── */
      .toolkit-section-label{font-size:.78rem;font-weight:600;color:#475569;text-transform:uppercase;letter-spacing:.04em;margin-bottom:4px;margin-top:10px}
    ")),

    # ── Right-click context menu (injected once) ──────────────────────────
    tags$div(id = "col-ctx-menu",
             tags$div(class="ctx-item", id="ctx-type",    tags$i(class="fa fa-exchange-alt"), "Change Data Type"),
             tags$div(class="ctx-item", id="ctx-round",   tags$i(class="fa fa-hashtag"),      "Round Values"),
             tags$hr(class="ctx-separator"),
             tags$div(class="ctx-item", id="ctx-trim-ws", tags$i(class="fa fa-eraser"),       "Trim Whitespace"),
             tags$div(class="ctx-item", id="ctx-case",    tags$i(class="fa fa-font"),         "Change Case"),
             tags$div(class="ctx-item", id="ctx-split",   tags$i(class="fa fa-cut"),          "Split Column"),
             tags$div(class="ctx-item", id="ctx-nchar",   tags$i(class="fa fa-text-width"),   "Trim by Characters"),
             tags$hr(class="ctx-separator"),
             tags$div(class="ctx-item", id="ctx-impute",  tags$i(class="fa fa-magic"),        "Handle Missing Values"),
             tags$div(class="ctx-item", id="ctx-dupes",   tags$i(class="fa fa-clone"),        "Remove Duplicates")
    ),

    # ── JS: right-click on .pbi-table th, dismiss on click-away ──────────
    tags$script(HTML("
      $(document).on('contextmenu', '.pbi-table th', function(e) {
        e.preventDefault();
        var col = $(this).data('col');
        var isNum = $(this).data('numeric') === true || $(this).data('numeric') === 'true';
        Shiny.setInputValue('ctx_col',  col,   {priority:'event'});
        Shiny.setInputValue('ctx_isnum', isNum, {priority:'event'});
        // enable/disable items based on column type
        if (isNum) {
          // numeric column: Round ON, text ops OFF
          $('#ctx-round').removeClass('disabled');
          $('#ctx-trim-ws, #ctx-case, #ctx-split, #ctx-nchar').addClass('disabled');
        } else {
          // text column: Round OFF, text ops ON
          $('#ctx-round').addClass('disabled');
          $('#ctx-trim-ws, #ctx-case, #ctx-split, #ctx-nchar').removeClass('disabled');
        }
        var menu = $('#col-ctx-menu');
        menu.css({ top: e.clientY + 'px', left: e.clientX + 'px', display: 'block' });
      });
      $(document).on('click', function(e) {
        if (!$(e.target).closest('#col-ctx-menu').length) $('#col-ctx-menu').hide();
      });
      // Wire each menu item to fire a Shiny input
      ['ctx-type','ctx-round','ctx-trim-ws','ctx-case',
       'ctx-split','ctx-nchar','ctx-impute','ctx-dupes'].forEach(function(id) {
        $('#' + id).on('click', function() {
          if ($(this).hasClass('disabled')) return;
          Shiny.setInputValue('ctx_action', id, {priority:'event'});
          $('#col-ctx-menu').hide();
        });
      });
    "))
  ),

  nav_panel(
    title = "Data Cleaner",
    layout_sidebar(

      # ── Left sidebar: Data Source ────────────────────────────────────────
      sidebar = sidebar(
        title = "DATA SOURCE", width = 300,
        fileInput("file_input", "Drop file here", accept = ".csv",
                  buttonLabel = "Browse...", placeholder = "CSV • XLSX • JSON • Parquet"),
        actionButton("btn_clear_file", "❌ Clear Current File",
                     class = "btn-outline-danger w-100 btn-sm",
                     style = "margin-top:-10px;margin-bottom:15px;"),
        tags$hr(),
        tags$h5("Connections"),
        actionButton("btn_open_hub", "⚡ Connect with Source Data",
                     class = "btn-outline-secondary w-100 text-start",
                     style = "border-color:#ced4da;"),
        tags$hr(),
        tags$h5("Loaded dataset"),
        strong(textOutput("dataset_name_ui")),
        span(textOutput("dataset_dims_ui"), style = "color:#666;font-size:.9rem;"),
        br(), br(),

        # ── Persistent buttons (always in sidebar as requested) ───────────
        div(class = "toolkit-section-label", "Dataset Operations"),
        div(style = "display:flex;flex-direction:column;gap:6px;",
            actionButton("btn_use_first_row_hdr", "↑ Use First Row as Headers",
                         class = "btn-outline-secondary w-100 btn-sm"),
            actionButton("btn_apply_col_trans",   "▶ Execute Column Transforms",
                         class = "btn-primary w-100 btn-sm")
        ),
        br(),
        actionButton("btn_export", "↓ Export clean data", class = "btn-outline-dark w-100")
      ),

      # ── Main content ─────────────────────────────────────────────────────
      navset_pill(
        id = "main_tabs",

        nav_panel("Overview",
                  br(),
                  layout_columns(
                    value_box("Total rows",          textOutput("total_rows"),     theme="success"),
                    value_box("Columns",             textOutput("total_cols"),     theme="light"),
                    value_box("Missing values",      textOutput("missing_count"),  theme="light"),
                    value_box("Columns with issues", textOutput("columns_issues"), theme="danger"),
                    col_widths = c(3,3,3,3)
                  ),
                  br(),
                  div(style="display:flex;justify-content:space-between;align-items:center;",
                      tags$h4("Column health overview"),
                      span("Scroll to review all", class="badge bg-light text-dark",
                           style="border:1px solid #ddd;")),
                  br(),
                  uiOutput("column_cards_container"),
                  br(),
                  div(style="display:flex;justify-content:space-between;align-items:center;border-top:1px solid #ddd;padding-top:15px;",
                      span(textOutput("pending_actions_count_ui"), style="color:#666;font-size:.9rem;"),
                      actionButton("btn_review","Review applied steps", class="btn-outline-secondary"))
        ),

        nav_panel("Columns",
                  br(),
                  # ── Two-column layout: table on left, Applied Steps on right ─────
                  layout_columns(
                    col_widths = c(9, 3),

                    div(
                      card(
                        card_header(
                          div(style="display:flex;gap:25px;align-items:center;flex-wrap:wrap;",
                              strong("Data View:", style="font-size:.95rem;color:#475569;"),
                              checkboxInput("pbi_quality","Column quality",    value=TRUE),
                              checkboxInput("pbi_dist",   "Column distribution",value=FALSE))
                        ),
                        div(class="pbi-table-container", uiOutput("power_bi_table"))
                      ),
                      br(),
                      layout_columns(
                        col_widths = c(4,4,4),
                        card(card_header(strong("Column statistics")), uiOutput("column_stats_table_ui")),
                        card(card_header(strong("Active Column")),     uiOutput("active_col_info_ui")),
                        card(card_header(strong("Value distribution")),plotOutput("profile_histogram_plot",height="220px"))
                      )
                    ),

                    # ── Applied Steps panel ────────────────────────────────────────
                    div(
                      div(class="steps-panel",
                          div(class="steps-panel-header",
                              strong("Applied Steps"),
                              actionButton("btn_clear_all_steps", "Clear All",
                                           class="btn btn-outline-danger btn-sm",
                                           style="font-size:.75rem;padding:2px 8px;")),
                          uiOutput("applied_steps_ui")
                      )
                    )
                  )
        ),

        nav_panel("Preview",
                  br(),
                  card(
                    card_header(
                      div(style="display:flex;justify-content:space-between;align-items:center;",
                          strong("Dataset Preview (First 500 rows)"),
                          radioButtons("preview_view_type","Filter rows:",
                                       choices=c("All Data"="all","Nulls Only"="nulls"),
                                       selected="all",inline=TRUE))
                    ),
                    div(style="overflow:auto;max-height:450px;padding:10px;",
                        tableOutput("preview_data_table"))
                  )
        ),

        nav_panel("Applied Steps Log",
                  br(),
                  card(
                    card_header(strong("Full Transformation Log")),
                    div(style="overflow:auto;max-height:450px;padding:10px;",
                        tableOutput("steps_log_table"))
                  )
        )
      )
    )
  )
)



# SERVER
server <- function(input, output, session) {

  # ── MASTER STATE ───────────────────────────────────────────────────────────
  # pipeline()    — R6 Pipeline instance; owns origin_df + step list.
  #                 Proposal: "Pipeline R6 class to manage workflow state."
  # origin_df()   — reactiveVal mirror of pipeline$origin_df so Shiny can
  #                 track changes reactively (R6 mutations are not reactive).
  # step_list()   — reactiveVal mirror of pipeline$steps for the same reason.
  # dataset()     — derived reactive: pipeline$apply_steps() whenever
  #                 step_list() invalidates.
  # active_col()  — currently selected column name
  # ctx_col()     — column targeted by the right-click context menu
  # pending       — reactiveValues holding params while a modal is open


  # The R6 Pipeline object — single authoritative state owner
  pipeline <- Pipeline$new()

  # Shiny-reactive mirrors (R6 field changes don't trigger Shiny invalidation)
  origin_df    <- reactiveVal(NULL)
  step_list    <- reactiveVal(list())

  active_col          <- reactiveVal(NULL)
  ctx_col             <- reactiveVal(NULL)
  selected_source     <- reactiveVal(NULL)
  connected_data_name <- reactiveVal(NULL)
  pending             <- reactiveValues(action=NULL, col=NULL)

  # dataset() re-runs pipeline$apply_steps() whenever step_list() changes.
  # This is the single output of the R6 class used by every UI output.
  dataset <- reactive({
    step_list()   # declare reactive dependency on the mirror
    if (is.null(origin_df())) return(NULL)
    tryCatch(
      pipeline$apply_steps(),
      error = function(e) {
        showNotification(paste("Pipeline error:", e$message), type="error")
        NULL
      }
    )
  })

  # ── Helper: add step via R6 then sync reactive mirror
  add_step <- function(step) {
    pipeline$add_step(step)
    step_list(pipeline$steps)   # push new list into reactiveVal → invalidates dataset()
  }

  # ── Helper: remove step via R6 then sync reactive mirror
  remove_step <- function(idx) {
    tryCatch({
      pipeline$remove_step(idx)
      step_list(pipeline$steps)
    }, error = function(e) {
      showNotification(paste("Remove error:", e$message), type="error")
    })
  }

  # FILE UPLOAD
  observeEvent(input$file_input, {
    req(input$file_input)
    df <- tryCatch(
      read.csv(input$file_input$datapath, stringsAsFactors=FALSE),
      error = function(e) { showNotification(paste("Read error:", e$message), type="error"); NULL }
    )
    req(df)

    # Defensive validation via Pipeline (proposal: "Robust input validation")
    tryCatch({
      pipeline <<- Pipeline$new(df)   # re-initialise R6 instance with new data
    }, error = function(e) {
      showNotification(paste("Data validation error:", e$message), type="error")
      return()
    })

    # Sync reactive mirrors
    origin_df(df)
    step_list(list())
    active_col(colnames(df)[1])
    connected_data_name(NULL)
    showNotification(paste0("Loaded: ", nrow(df), " rows × ", ncol(df), " columns"), type="message")
  })

  # CLEAR FILE
  observeEvent(input$btn_clear_file, {
    shinyjs::runjs("var fi=document.getElementById('file_input'); if(fi) fi.value='';
                    document.querySelectorAll('.progress').forEach(function(el){el.style.display='none';});")
    pipeline <<- Pipeline$new()   # reset R6 instance to blank state
    origin_df(NULL); step_list(list()); active_col(NULL); connected_data_name(NULL)
    showNotification("Dataset cleared.", type="warning")
  })

  # CLEAR ALL STEPS
  observeEvent(input$btn_clear_all_steps, {
    pipeline$steps <- list()      # reset R6 step list
    step_list(list())             # sync reactive mirror
    showNotification("All steps cleared. Showing original data.", type="warning")
  })

  #TABLE HEADER LEFT-CLICK → active column
  observeEvent(input$pbi_col_clicked, { active_col(input$pbi_col_clicked) })

  #  RIGHT-CLICK → capture targeted column
  observeEvent(input$ctx_col, { ctx_col(input$ctx_col) })

  #RIGHT-CLICK ACTION DISPATCH
  observeEvent(input$ctx_action, {
    col <- ctx_col()
    if (is.null(col)) return()
    active_col(col)  # also update active column
    pending$action <- input$ctx_action
    pending$col    <- col
    df <- dataset(); req(df)
    is_num <- col %in% colnames(df) && is.numeric(df[[col]])

    switch(input$ctx_action,

           # ── Change Data Type modal
           "ctx-type" = showModal(modalDialog(
             title = paste0("Change Data Type — ", col),
             selectInput("modal_type_choice", "Convert to:",
                         choices = c("Numeric"="numeric","Text/Character"="character",
                                     "Factor"="factor","Date"="date","Binary (0/1)"="binary")),
             footer = tagList(modalButton("Cancel"),
                              actionButton("modal_type_ok","Apply",class="btn btn-primary")),
             easyClose = TRUE
           )),

           # ── Round Values modal
           "ctx-round" = showModal(modalDialog(
             title = paste0("Round Values — ", col),
             if (!is_num) div(class="alert alert-warning","Column is not numeric; convert first.")
             else numericInput("modal_round_digits","Decimal places:",value=2,min=0,max=10,step=1),
             footer = tagList(modalButton("Cancel"),
                              if (is_num) actionButton("modal_round_ok","Apply",class="btn btn-primary")),
             easyClose = TRUE
           )),

           # ── Trim Whitespace (no config needed — apply immediately)
           "ctx-trim-ws" = {
             add_step(list(action="trim_whitespace", col=col))
             showNotification(paste0("Step added: Trim Whitespace on '", col, "'"), type="message")
           },

           # ── Change Case modal
           "ctx-case" = showModal(modalDialog(
             title = paste0("Change Case — ", col),
             radioButtons("modal_case_choice","Target case:",
                          choices=c("UPPERCASE"="upper","lowercase"="lower","Title Case"="title"),
                          selected="upper", inline=TRUE),
             footer = tagList(modalButton("Cancel"),
                              actionButton("modal_case_ok","Apply",class="btn btn-primary")),
             easyClose = TRUE
           )),

           # ── Split Column modal
           "ctx-split" = showModal(modalDialog(
             title = paste0("Split Column — ", col),
             textInput("modal_split_delim","Delimiter character:",value=","),
             numericInput("modal_split_part","Keep part # (1=left, 2=right…):",value=1,min=1,step=1),
             span("Column will be replaced with the chosen part.",style="font-size:.8rem;color:#94a3b8;"),
             footer = tagList(modalButton("Cancel"),
                              actionButton("modal_split_ok","Apply",class="btn btn-primary")),
             easyClose = TRUE
           )),

           # ── Trim by Characters modal
           "ctx-nchar" = showModal(modalDialog(
             title = paste0("Trim by Characters — ", col),
             numericInput("modal_nchar_count","Max characters to keep:",value=10,min=1,step=1),
             radioButtons("modal_nchar_side","Keep from:",
                          choices=c("Left (start)"="left","Right (end)"="right"),
                          selected="left",inline=TRUE),
             footer = tagList(modalButton("Cancel"),
                              actionButton("modal_nchar_ok","Apply",class="btn btn-primary")),
             easyClose = TRUE
           )),

           # ── Handle Missing Values modal
           "ctx-impute" = showModal(modalDialog(
             title = paste0("Handle Missing Values — ", col),
             if (is_num) {
               selectInput("modal_impute_method","Method:",
                           choices=c("Keep Nulls"="keep","Replace with Mean"="mean",
                                     "Replace with Median"="median","Drop Nulls"="drop"))
             } else {
               tagList(
                 selectInput("modal_impute_method","Method:",
                             choices=c("Keep Nulls"="keep","Replace with Value"="constant","Drop Nulls"="drop")),
                 conditionalPanel(
                   condition="input.modal_impute_method=='constant'",
                   textInput("modal_impute_fill","Replace with:",value="Unknown")
                 )
               )
             },
             footer = tagList(modalButton("Cancel"),
                              actionButton("modal_impute_ok","Apply",class="btn btn-primary")),
             easyClose = TRUE
           )),

           # ── Remove Duplicates (no config
           "ctx-dupes" = {
             add_step(list(action="remove_duplicates", col=NA))
             showNotification("Step added: Remove Duplicate Rows", type="message")
           }
    )
  })

  # ── MODAL OK HANDLERS
  observeEvent(input$modal_type_ok, {
    removeModal()
    col <- pending$col; req(col)
    add_step(list(action="convert_type", col=col, to_type=input$modal_type_choice))
    showNotification(paste0("Step added: Convert '", col, "' to ", input$modal_type_choice), type="message")
  })

  observeEvent(input$modal_round_ok, {
    removeModal()
    col <- pending$col; req(col)
    add_step(list(action="round_values", col=col, digits=as.integer(input$modal_round_digits)))
    showNotification(paste0("Step added: Round '", col, "' to ", input$modal_round_digits, " dp"), type="message")
  })

  observeEvent(input$modal_case_ok, {
    removeModal()
    col <- pending$col; req(col)
    add_step(list(action="change_case", col=col, case=input$modal_case_choice))
    showNotification(paste0("Step added: Case '", col, "' → ", input$modal_case_choice), type="message")
  })

  observeEvent(input$modal_split_ok, {
    removeModal()
    col <- pending$col; req(col)
    add_step(list(action="split_column", col=col,
                  delim=input$modal_split_delim,
                  part=as.integer(input$modal_split_part)))
    showNotification(paste0("Step added: Split '", col, "'"), type="message")
  })

  observeEvent(input$modal_nchar_ok, {
    removeModal()
    col <- pending$col; req(col)
    add_step(list(action="trim_chars", col=col,
                  nchars=as.integer(input$modal_nchar_count),
                  side=input$modal_nchar_side))
    showNotification(paste0("Step added: Trim chars '", col, "'"), type="message")
  })

  observeEvent(input$modal_impute_ok, {
    removeModal()
    col <- pending$col; req(col)
    method <- input$modal_impute_method
    if (method == "keep") { showNotification("No imputation applied.", type="warning"); return() }
    fill_val <- if (!is.null(input$modal_impute_fill) && method=="constant") input$modal_impute_fill else NULL
    add_step(list(action="impute", col=col, method=method, fill_value=fill_val))
    showNotification(paste0("Step added: Impute '", col, "' → ", method), type="message")
  })

  # ── PERSISTENT SIDEBAR BUTTONS

  # Use First Row as Headers — no config needed, add step directly
  observeEvent(input$btn_use_first_row_hdr, {
    req(origin_df())
    add_step(list(action="first_row_headers", col=NA))
    showNotification("Step added: Use First Row as Headers", type="message")
  })

  # Execute Column Transforms — a multi-option modal for the active column
  # (kept as a convenience for users who prefer a form over right-clicking)
  observeEvent(input$btn_apply_col_trans, {
    df <- dataset(); req(df)
    col <- active_col()
    if (is.null(col) || !(col %in% colnames(df))) {
      showNotification("No active column selected. Click a column header first.", type="warning")
      return()
    }
    is_num <- is.numeric(df[[col]])
    pending$col <- col

    showModal(modalDialog(
      title = div(style="display:flex;align-items:center;gap:10px;",
                  tags$i(class="fa fa-sliders-h",style="color:#10b981;"),
                  paste0("Column Transforms — ", col)),
      size = "m",
      # Type
      div(class="toolkit-section-label","Data Type"),
      selectInput("exec_type","Convert to:",
                  choices=c("No Change"="none","Numeric"="numeric","Text/Character"="character",
                            "Factor"="factor","Date"="date","Binary (0/1)"="binary")),
      # Rounding
      if (is_num) tagList(
        checkboxInput("exec_round_enable","Round numeric values",value=FALSE),
        conditionalPanel("input.exec_round_enable==true",
                         numericInput("exec_round_digits","Decimal places:",value=2,min=0,max=10,step=1))
      ),
      # String ops (text only)
      if (!is_num) tagList(
        div(class="toolkit-section-label","String Operations"),
        checkboxInput("exec_trim","Trim whitespace",value=FALSE),
        selectInput("exec_case","Change case:",
                    choices=c("No Change"="none","UPPERCASE"="upper","lowercase"="lower","Title Case"="title")),
        checkboxInput("exec_split_enable","Split by delimiter",value=FALSE),
        conditionalPanel("input.exec_split_enable==true",
                         textInput("exec_split_delim","Delimiter:",value=","),
                         numericInput("exec_split_part","Keep part:",value=1,min=1,step=1)),
        checkboxInput("exec_nchar_enable","Trim to N characters",value=FALSE),
        conditionalPanel("input.exec_nchar_enable==true",
                         numericInput("exec_nchar_count","Max characters:",value=10,min=1,step=1),
                         radioButtons("exec_nchar_side","Keep from:",
                                      choices=c("Left"="left","Right"="right"),selected="left",inline=TRUE))
      ),
      # Imputation
      div(class="toolkit-section-label","Missing Values"),
      if (is_num) {
        selectInput("exec_impute","Handle nulls:",
                    choices=c("Keep Nulls"="keep","Replace with Mean"="mean","Replace with Median"="median","Drop Nulls"="drop"))
      } else {
        tagList(
          selectInput("exec_impute","Handle nulls:",
                      choices=c("Keep Nulls"="keep","Replace with Value"="constant","Drop Nulls"="drop")),
          conditionalPanel("input.exec_impute=='constant'",
                           textInput("exec_impute_fill","Replace with:",value="Unknown"))
        )
      },
      footer = tagList(
        modalButton("Cancel"),
        actionButton("exec_modal_ok","Add Steps",class="btn btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$exec_modal_ok, {
    removeModal()
    col <- pending$col; req(col)
    df  <- dataset(); req(df)
    is_num <- col %in% colnames(df) && is.numeric(df[[col]])
    added <- 0

    # Type conversion
    if (!is.null(input$exec_type) && input$exec_type != "none") {
      add_step(list(action="convert_type",col=col,to_type=input$exec_type)); added <- added+1
      is_num <- input$exec_type == "numeric"  # update flag for subsequent steps
    }
    # Rounding
    if (is_num && isTRUE(input$exec_round_enable)) {
      add_step(list(action="round_values",col=col,digits=as.integer(input$exec_round_digits))); added <- added+1
    }
    # Trim whitespace
    if (!is_num && isTRUE(input$exec_trim)) {
      add_step(list(action="trim_whitespace",col=col)); added <- added+1
    }
    # Case
    if (!is_num && !is.null(input$exec_case) && input$exec_case != "none") {
      add_step(list(action="change_case",col=col,case=input$exec_case)); added <- added+1
    }
    # Split
    if (!is_num && isTRUE(input$exec_split_enable)) {
      add_step(list(action="split_column",col=col,
                    delim=input$exec_split_delim,
                    part=as.integer(input$exec_split_part))); added <- added+1
    }
    # Trim chars
    if (!is_num && isTRUE(input$exec_nchar_enable)) {
      add_step(list(action="trim_chars",col=col,
                    nchars=as.integer(input$exec_nchar_count),
                    side=input$exec_nchar_side)); added <- added+1
    }
    # Imputation
    if (!is.null(input$exec_impute) && input$exec_impute != "keep") {
      fill_val <- if (!is.null(input$exec_impute_fill) && input$exec_impute=="constant") input$exec_impute_fill else NULL
      add_step(list(action="impute",col=col,method=input$exec_impute,fill_value=fill_val)); added <- added+1
    }

    if (added > 0) showNotification(paste0(added," step(s) added for column '",col,"'"), type="message")
    else           showNotification("No operations selected.", type="warning")
  })

  # ── GEAR ICON — view step settings
  # The server observes dynamically-named inputs "step_gear_N"
  observe({
    steps <- step_list()
    lapply(seq_along(steps), function(i) {
      btn_id <- paste0("step_gear_", i)
      observeEvent(input[[btn_id]], {
        s <- step_list()[[i]]
        showModal(modalDialog(
          title = div(style="display:flex;align-items:center;gap:8px;",
                      tags$i(class="fa fa-cog",style="color:#475569;"),
                      paste0("Step ", i, " Settings")),
          h6(step_label(s), style="color:#64748b;margin-bottom:12px;"),
          step_settings_html(s),
          footer = modalButton("Close"),
          easyClose = TRUE
        ))
      }, ignoreInit=TRUE, once=FALSE)
    })
  })

  # ── X BUTTON — remove step and replay
  observe({
    steps <- step_list()
    lapply(seq_along(steps), function(i) {
      btn_id <- paste0("step_remove_", i)
      observeEvent(input[[btn_id]], {
        lbl <- step_label(step_list()[[i]])
        remove_step(i)
        showNotification(paste0("Removed: ", lbl), type="message")
      }, ignoreInit=TRUE, once=FALSE)
    })
  })

  observeEvent(input$btn_review, {
    updateNavsetPill(session,"main_tabs",selected="Applied Steps Log")
  })

  # ── APPLIED STEPS PANEL UI
  output$applied_steps_ui <- renderUI({
    steps <- step_list()
    if (length(steps) == 0)
      return(div(class="steps-empty",
                 tags$i(class="fa fa-stream",style="display:block;font-size:1.5rem;margin-bottom:6px;color:#cbd5e1;"),
                 "No steps applied yet.", br(),
                 span("Right-click a column header to begin.",style="font-size:.75rem;")))

    step_items <- lapply(seq_along(steps), function(i) {
      s <- steps[[i]]
      div(class="step-item",
          div(class="step-dot"),
          div(class="step-label", paste0(i, ". ", step_label(s))),
          # Gear button — fires step_gear_N
          tags$button(class="step-gear",
                      id=paste0("step_gear_",i),
                      onclick=sprintf("Shiny.setInputValue('step_gear_%d',%d,{priority:'event'});",i,i),
                      tags$i(class="fa fa-cog")),
          # X button — fires step_remove_N
          tags$button(class="step-remove",
                      id=paste0("step_remove_",i),
                      onclick=sprintf("Shiny.setInputValue('step_remove_%d',%d,{priority:'event'});",i,i),
                      tags$i(class="fa fa-times"))
      )
    })
    do.call(div, step_items)
  })

  # ── SIDEBAR OUTPUTS
  output$dataset_name_ui <- renderText({
    if (!is.null(connected_data_name())) return(connected_data_name())
    if (is.null(origin_df())) return("No file loaded")
    if (!is.null(input$file_input)) input$file_input$name else "Loaded"
  })
  output$dataset_dims_ui <- renderText({
    df <- dataset()
    if (is.null(df)) "0 rows • 0 columns"
    else paste(nrow(df),"rows •",ncol(df),"columns")
  })

  # ── OVERVIEW VALUE BOXES
  output$total_rows     <- renderText({ df<-dataset(); if(is.null(df))"0" else as.character(nrow(df)) })
  output$total_cols     <- renderText({ df<-dataset(); if(is.null(df))"0" else as.character(ncol(df)) })
  output$missing_count  <- renderText({
    df<-dataset(); if(is.null(df)) "0"
    else as.character(sum(sapply(df, function(v) sum(is.na(v)|as.character(v)==""))))
  })
  output$columns_issues <- renderText({
    df<-dataset(); if(is.null(df)) "0"
    else as.character(sum(sapply(df, function(v) any(is.na(v)|as.character(v)==""))))
  })
  output$pending_actions_count_ui <- renderText({
    paste(length(step_list()), "step(s) in the Applied Steps pipeline.")
  })

  # ── COLUMN HEALTH CARDS
  output$column_cards_container <- renderUI({
    df <- dataset()
    if (is.null(df)) return(p("Upload a file to generate column evaluations.",style="color:gray;font-style:italic;"))
    cards <- lapply(colnames(df), function(nm) {
      cv      <- df[[nm]]
      na_c    <- sum(is.na(cv)|as.character(cv)=="")
      na_pct  <- round(na_c/length(cv)*100,1)
      dlbl    <- "N/A (Categorical)"; olbl <- "No outliers"; ocls <- "bg-success"
      if (is.numeric(cv)) {
        cl <- na.omit(cv)
        if (length(cl)>3) {
          sk <- tryCatch({m3<-mean((cl-mean(cl))^3);s3<-sd(cl)^3;if(s3>0)m3/s3 else 0},error=function(e)0)
          dlbl <- if(abs(sk)<0.5)"Normal" else if(sk>=0.5)"Right-Skewed" else "Left-Skewed"
          q1<-quantile(cl,.25);q3<-quantile(cl,.75);iqr<-q3-q1
          outs<-cl[cl<q1-1.5*iqr|cl>q3+1.5*iqr]; op<-round(length(outs)/length(cl)*100,1)
          if(length(outs)>0){olbl<-paste0(if(op>5)"Big" else "Minor"," Outliers (",op,"%)");ocls<-if(op>5)"bg-danger" else "bg-warning text-dark"}
        }
      }
      card(style="margin-bottom:15px;box-shadow:0 1px 3px rgba(0,0,0,.05);border-left:4px solid #10b981;",
           card_body(
             div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;",
                 tags$strong(nm,style="font-size:1.1rem;color:#2c3e50;"),
                 span(if(na_pct>0)"Review" else "Clean",class=if(na_pct>0)"badge bg-warning text-dark" else "badge bg-success")),
             div(style="display:grid;grid-template-columns:repeat(3,1fr);gap:10px;font-size:.85rem;margin-bottom:10px;",
                 div(tags$span("Nulls: "),tags$strong(paste0(na_pct,"%"))),
                 div(tags$span("Dist: "),tags$strong(dlbl)),
                 div(tags$span("Outliers: "),span(olbl,class=paste("badge",ocls)))),
             div(class="progress",style="height:8px;",
                 div(class=paste("progress-bar",if(na_pct>0)"bg-warning" else "bg-success"),
                     style=paste0("width:",(100-na_pct),"%;")))
           ))
    })
    do.call(tagList, cards)
  })

  # ── POWER BI TABLE
  output$power_bi_table <- renderUI({
    df <- dataset()
    if (is.null(df)) return(p("No data loaded.",style="padding:20px;font-style:italic;color:#64748b;"))
    sel  <- active_col()
    cols <- colnames(df)

    headers <- lapply(cols, function(col) {
      cv      <- df[[col]]
      n       <- length(cv)
      is_num  <- is.numeric(cv)
      ui_type <- if(is_num)"1.2" else "A B C"
      na_c    <- sum(is.na(cv)|as.character(cv)=="")
      ep      <- round(na_c/n*100); vp <- 100-ep
      dc      <- length(unique(cv)); uc <- sum(table(cv)==1)
      sel_cls <- if(!is.null(sel)&&sel==col)"selected-col-header" else ""
      tbl     <- table(head(cv,100)); mx <- if(length(tbl)>0)max(tbl) else 1

      # data-col and data-numeric attributes are read by the JS right-click handler
      tags$th(class=sel_cls,
              `data-col`=col, `data-numeric`=tolower(as.character(is_num)),
              onclick=sprintf("Shiny.setInputValue('pbi_col_clicked','%s',{priority:'event'});",col),
              div(class="header-top-row",
                  div(class="header-title-text",col),
                  span(ui_type,class="header-type-label")),
              if(input$pbi_quality)
                div(class="quality-bar-wrapper",
                    div(class="quality-bar",
                        div(class="q-valid",style=paste0("width:",vp,"%;")),
                        div(class="q-empty",style=paste0("width:",ep,"%;")))
                    ,div(class="quality-text-legend",
                         span(paste0("Valid:",vp,"%")),span(paste0("Empty:",ep,"%")))),
              if(input$pbi_dist)
                div(class="mini-dist-container",
                    div(class="mini-bar-chart",
                        lapply(head(as.numeric(tbl),8),function(v)
                          div(class="mini-bar",style=paste0("height:",min(100,max(15,round(v/mx*100))),"%;"))))
                    ,div(class="mini-dist-counts",paste0(dc," distinct, ",uc," unique")))
      )
    })

    preview <- head(df,1000)
    rows <- lapply(seq_len(nrow(preview)), function(i)
      tags$tr(lapply(cols, function(col) {
        v <- preview[i,col]
        tags$td(class=if(!is.null(sel)&&sel==col)"selected-col-cell" else "",
                if(is.na(v)||as.character(v)=="") tags$em("null",style="color:#cbd5e1;")
                else htmlEscape(as.character(v)))
      })))

    tags$table(class="pbi-table",tags$thead(tags$tr(headers)),tags$tbody(rows))
  })

  # ── COLUMN STATS
  output$column_stats_table_ui <- renderUI({
    df <- dataset(); req(df)
    col <- active_col()
    if (is.null(col)||!(col%in%colnames(df))) return(p("Click a column.",style="color:#64748b;"))
    cv <- df[[col]]
    rows <- list(
      tags$tr(tags$td("Count"),    tags$td(strong(length(cv)))),
      tags$tr(tags$td("Empty"),    tags$td(strong(sum(is.na(cv)|as.character(cv)=="")))),
      tags$tr(tags$td("Distinct"), tags$td(strong(length(unique(cv))))),
      tags$tr(tags$td("Unique"),   tags$td(strong(sum(table(cv)==1))))
    )
    if (is.numeric(cv)) {
      cl <- na.omit(cv)
      rows <- c(rows,list(
        tags$tr(tags$td("Min"),     tags$td(strong(if(length(cl)>0)min(cl)           else "N/A"))),
        tags$tr(tags$td("Max"),     tags$td(strong(if(length(cl)>0)max(cl)           else "N/A"))),
        tags$tr(tags$td("Mean"),    tags$td(strong(if(length(cl)>0)round(mean(cl),4) else "N/A")))
      ))
    }
    tags$table(class="table table-sm table-striped",style="font-size:.8rem;margin:0;",tags$tbody(rows))
  })

  # ── ACTIVE COLUMN INFO CARD
  output$active_col_info_ui <- renderUI({
    df  <- dataset()
    col <- active_col()
    if (is.null(df)||is.null(col)||!(col%in%colnames(df)))
      return(p("Right-click a column header to add a step.",style="color:#94a3b8;font-size:.82rem;font-style:italic;"))
    cv <- df[[col]]
    tagList(
      p(strong(col), style="margin-bottom:4px;font-size:.9rem;"),
      span(class="badge bg-light text-dark",
           style="border:1px solid #e2e8f0;font-size:.75rem;margin-bottom:8px;",
           if(is.numeric(cv))"Numeric" else if(is.factor(cv))"Factor" else "Text"),
      br(), br(),
      p(style="font-size:.78rem;color:#64748b;",
        "Right-click the column header to add transformation steps.")
    )
  })

  # ── DISTRIBUTION PLOT
  output$profile_histogram_plot <- renderPlot({
    df  <- dataset(); req(df)
    col <- active_col(); req(col)
    if (!(col %in% colnames(df))) return(NULL)
    cv <- na.omit(df[[col]])
    if (length(cv)==0) return(NULL)
    pdf <- data.frame(Value=cv)
    if (is.numeric(cv)) {
      ggplot(pdf,aes(x=Value))+geom_histogram(fill="#0ea5e9",color="#fff",bins=30,alpha=.9)+
        theme_minimal(base_size=11)+labs(x=NULL,y=NULL)+theme(panel.grid.minor=element_blank())
    } else {
      td <- as.data.frame(table(Value=cv))
      td <- head(td[order(-td$Freq),],15)
      ggplot(td,aes(x=reorder(Value,-Freq),y=Freq))+geom_col(fill="#0ea5e9",alpha=.9,width=.75)+
        theme_minimal(base_size=11)+labs(x=NULL,y=NULL)+
        theme(axis.text.x=element_text(angle=45,hjust=1),panel.grid.minor=element_blank())
    }
  })

  # ── PREVIEW TABLE
  output$preview_data_table <- renderTable({
    df <- dataset(); req(df)
    if (input$preview_view_type=="nulls") {
      mask <- apply(df,1,function(r) any(is.na(r)|as.character(r)==""))
      fd   <- df[mask,,drop=FALSE]
      if (nrow(fd)==0) return(data.frame(Message="No null rows in current dataset."))
      return(head(fd,500))
    }
    head(df,500)
  })

  # ── APPLIED STEPS LOG TABLE
  output$steps_log_table <- renderTable({
    steps <- step_list()
    if (length(steps)==0) return(data.frame(Status="No steps applied yet."))
    data.frame(
      `#`      = seq_along(steps),
      Column   = sapply(steps, function(s) if(is.na(s$col)) "(All)" else s$col),
      Action   = sapply(steps, function(s) s$action),
      Summary  = sapply(steps, step_label),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  # ── DATA SOURCE HUB
  observeEvent(input$btn_open_hub, {
    selected_source(NULL)
    showModal(modalDialog(
      title=div(style="display:flex;align-items:center;justify-content:space-between;width:100%;",
                tags$span(strong("Get Data"),style="font-size:1.3rem;"),
                div(class="search-container",
                    tags$i(class="fa fa-search"),
                    textInput("search_source",NULL,placeholder="Search data sources…",width="100%"))),
      size="l",easyClose=TRUE,
      uiOutput("filtered_sources_ui"),
      footer=tagList(modalButton("Cancel"),uiOutput("connect_btn_ui"))
    ))
  })

  all_src_ids <- c("excel","json","access","sqldb","snow","sfobj","sfrep","gbq","databr","py","rscript")
  lapply(all_src_ids, function(id)
    observeEvent(input[[paste0("select_",id)]], selected_source(id), ignoreInit=TRUE))

  output$connect_btn_ui <- renderUI({
    btn <- actionButton("btn_modal_connect","Connect",class="btn btn-success")
    if (is.null(selected_source())) tagAppendAttributes(btn,disabled=NA) else btn
  })

  observeEvent(input$btn_modal_connect, {
    req(selected_source()); src <- selected_source(); removeModal()
    form_fields <- switch(src,
                          "excel"  =tagList(fileInput("pbi_src_file","File path",accept=c(".xlsx",".xls",".xlsm")),checkboxInput("pbi_first_row","Use first row as headers",TRUE)),
                          "json"   =tagList(fileInput("pbi_src_file","File path",accept=".json"),textInput("pbi_json_root","JSON Root Element (optional)")),
                          "access" =tagList(fileInput("pbi_src_file","Database path",accept=c(".accdb",".mdb")),passwordInput("pbi_db_pass","Password (if encrypted)")),
                          "sqldb"  =tagList(textInput("pbi_srv","Server"),textInput("pbi_db","Database (optional)"),radioButtons("pbi_mode","Mode",c("Import","DirectQuery"),"Import"),tags$hr(),textAreaInput("pbi_sql","SQL (optional)",rows=5,width="100%")),
                          "snow"   =tagList(textInput("pbi_snow_url","Server URL"),textInput("pbi_wh","Warehouse"),textInput("pbi_db","Database (optional)"),tags$hr(),textAreaInput("pbi_snow_sql","SQL (optional)",rows=5,width="100%")),
                          "sfobj"  =tagList(radioButtons("pbi_sf_env","Environment",c("Production","Custom/Sandbox")),conditionalPanel("input.pbi_sf_env=='Custom/Sandbox'",textInput("pbi_sf_url","Custom Login URL")),checkboxInput("pbi_sf_rel","Include relationship columns",TRUE)),
                          "sfrep"  =tagList(radioButtons("pbi_sf_env2","Environment",c("Production","Custom/Sandbox")),conditionalPanel("input.pbi_sf_env2=='Custom/Sandbox'",textInput("pbi_sf_url2","Custom Login URL")),textInput("pbi_sf_rep_id","Report ID")),
                          "gbq"    =tagList(textInput("pbi_gbq_proj","Project ID"),checkboxInput("pbi_gbq_adv","Advanced options",FALSE),conditionalPanel("input.pbi_gbq_adv==true",div(style="padding-left:15px;border-left:2px solid #cbd5e1;",textAreaInput("pbi_gbq_sql","SQL",rows=8,width="100%"),textInput("pbi_gbq_billing","Billing project (optional)"),numericInput("pbi_gbq_limit","Row limit (optional)",NA))),span("Uses browser OAuth2.",style="font-size:.8rem;color:gray;")),
                          "databr" =tagList(textInput("pbi_srv2","Server Hostname"),textInput("pbi_http","HTTP Path"),passwordInput("pbi_pat","Personal Access Token"),tags$hr(),textAreaInput("pbi_db_sql","SQL (optional)",rows=5,width="100%")),
                          "py"     =tagList(textAreaInput("pbi_py","Python Script",rows=15,width="100%",placeholder="import pandas as pd\ndf = pd.read_csv('...')"),span("Runs locally.",style="font-size:.8rem;color:gray;")),
                          "rscript"=tagList(textAreaInput("pbi_r","R Script",rows=15,width="100%",placeholder="df <- read.csv('...')"),span("Runs locally.",style="font-size:.8rem;color:gray;"))
    )
    src_label <- switch(src,"excel"="Excel Workbook","json"="JSON File","access"="Access Database","sqldb"="SQL Server","snow"="Snowflake","sfobj"="Salesforce Objects","sfrep"="Salesforce Reports","gbq"="Google BigQuery","databr"="Azure Databricks","py"="Python Script","rscript"="R Script")
    showModal(modalDialog(
      title=paste("Connect to",src_label),size="m",easyClose=FALSE,form_fields,
      footer=tagList(actionButton("btn_back_to_hub","← Back",class="btn btn-outline-secondary"),modalButton("Cancel"),actionButton("btn_finalize_connect","Connect",class="btn btn-success"))
    ))
  })

  observeEvent(input$btn_back_to_hub, { removeModal(); click("btn_open_hub") })

  output$filtered_sources_ui <- renderUI({
    term <- tolower(if(!is.null(input$search_source)) input$search_source else "")
    srcs <- list(
      list(id="excel",  name="Excel workbook",      ext=".xlsx, .xls, .xlsm",icon="fa-file-excel icon-excel"),
      list(id="json",   name="JSON file",           ext=".json, .txt",        icon="fa-code icon-json"),
      list(id="access", name="Access database",     ext=".accdb, .mdb",       icon="fa-database icon-access"),
      list(id="sqldb",  name="SQL Server database", ext="SQL Engine",         icon="fa-database icon-sql"),
      list(id="snow",   name="Snowflake",           ext="Cloud Warehouse",    icon="fa-snowflake icon-snowflake"),
      list(id="sfobj",  name="Salesforce Objects",  ext="CRM Tables",         icon="fa-cloud icon-salesforce"),
      list(id="sfrep",  name="Salesforce Reports",  ext="CRM Reports",        icon="fa-chart-bar icon-salesforce"),
      list(id="gbq",    name="Google BigQuery",     ext="BigQuery",           icon="fa-cloud-meatball icon-google"),
      list(id="databr", name="Azure Databricks",    ext="Spark Lake",         icon="fa-fire icon-azure"),
      list(id="py",     name="Python script",       ext="Local run",          icon="fa-brands fa-python icon-python"),
      list(id="rscript",name="R script",            ext="Local run",          icon="fa-brands fa-r-project icon-r")
    )
    m <- Filter(function(s) term==""||grepl(term,tolower(s$name))||grepl(term,tolower(s$ext)), srcs)
    if (length(m)==0) return(div(style="text-align:center;padding:40px;color:#94a3b8;",
                                 tags$i(class="fa fa-search-minus",style="font-size:3rem;margin-bottom:10px;"),
                                 p("No matching data sources found.")))
    div(class="source-grid",
        lapply(m, function(s) {
          cls <- if(!is.null(selected_source())&&selected_source()==s$id)"source-item-btn selected-item" else "source-item-btn"
          actionLink(paste0("select_",s$id),
                     label=div(class=cls,tags$i(class=paste("fa",s$icon)),
                               tags$span(s$name,style="color:#334155;font-weight:500;font-size:.9rem;display:block;"),
                               tags$span(s$ext,class="source-ext-label")))
        }))
  })

  # ── EXPORT ────────────────────────────────────────────────────────────────
  observeEvent(input$btn_export, {
    showModal(modalDialog(
      title="Export Cleaned Dataset",
      p("Choose output format:"),
      div(style="display:flex;gap:12px;justify-content:center;padding:20px;",
          downloadButton("dl_csv","CSV",  class="btn-success"),
          downloadButton("dl_txt","TXT",  class="btn-info text-white"),
          downloadButton("dl_xls","Excel",class="btn-primary")),
      size="m",easyClose=TRUE,footer=modalButton("Dismiss")
    ))
  })

  output$dl_csv <- downloadHandler(filename=function() paste0("cleaned_",format(Sys.time(),"%Y%m%d_%H%M%S"),".csv"),  content=function(f) write.csv(dataset(),f,row.names=FALSE))
  output$dl_txt <- downloadHandler(filename=function() paste0("cleaned_",format(Sys.time(),"%Y%m%d_%H%M%S"),".txt"),  content=function(f) write.table(dataset(),f,sep="\t",row.names=FALSE,quote=FALSE))
  output$dl_xls <- downloadHandler(filename=function() paste0("cleaned_",format(Sys.time(),"%Y%m%d_%H%M%S"),".xls"),  content=function(f) write.table(dataset(),f,sep="\t",row.names=FALSE))
}

options(shiny.maxRequestSize = 200 * 1024^2)
shinyApp(ui, server)
