#' Launch the autoMLR Shiny app
#'
#' Starts a packaged Shiny dashboard for uploading a CSV file, applying common
#' data-cleaning transformations, previewing the cleaned data, and viewing a
#' simple distribution plot for the selected column.
#'
#' @param ... Additional arguments passed to [shiny::runApp()], such as
#'   `launch.browser = TRUE`.
#'
#' @return This function is called for its side effect of launching a Shiny app.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(...) {
  app <- shiny::shinyApp(ui = automlr_ui(), server = automlr_server)
  shiny::runApp(app, ...)
}

#' Build the autoMLR Shiny UI
#'
#' Creates the user interface for the packaged dashboard. The original project
#' script remains available in the repository; this function provides a compact
#' package-native app that exercises the same reusable pipeline functions.
#'
#' @return A Shiny UI object.
#' @export
automlr_ui <- function() {
  bslib::page_sidebar(
    title = "autoMLR",
    theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
    sidebar = bslib::sidebar(
      title = "Data source",
      shiny::fileInput("file", "Upload CSV", accept = ".csv"),
      shiny::selectInput("column", "Column", choices = character()),
      shiny::selectInput(
        "action",
        "Transformation",
        choices = c(
          "Preview only" = "none",
          "Trim whitespace" = "trim_whitespace",
          "Change to uppercase" = "upper",
          "Change to lowercase" = "lower",
          "Impute numeric mean" = "mean",
          "Impute numeric median" = "median",
          "Scale numeric column" = "scale_numeric",
          "Remove duplicate rows" = "remove_duplicates"
        )
      ),
      shiny::actionButton("add_step", "Add step", class = "btn-primary"),
      shiny::actionButton("clear_steps", "Clear steps", class = "btn-outline-danger"),
      shiny::hr(),
      shiny::downloadButton("download_clean", "Download cleaned CSV")
    ),
    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(
        bslib::card_header("Cleaned data preview"),
        shiny::tableOutput("preview")
      ),
      bslib::card(
        bslib::card_header("Applied steps"),
        shiny::uiOutput("steps"),
        shiny::plotOutput("distribution", height = 250)
      )
    )
  )
}

#' Shiny server for the autoMLR app
#'
#' Server function used by [run_app()]. It keeps the original uploaded data
#' immutable and replays transformation steps through [replay_steps()].
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return `NULL`; called by Shiny for side effects.
#' @export
automlr_server <- function(input, output, session) {
  origin_df <- shiny::reactiveVal(NULL)
  pipeline <- shiny::reactiveVal(Pipeline$new())
  steps <- shiny::reactiveVal(list())

  shiny::observeEvent(input$file, {
    shiny::req(input$file)
    df <- utils::read.csv(input$file$datapath, stringsAsFactors = FALSE)
    origin_df(df)
    pipeline(Pipeline$new(df))
    steps(list())
    shiny::updateSelectInput(session, "column", choices = names(df), selected = names(df)[1])
  })

  cleaned_data <- shiny::reactive({
    df <- origin_df()
    if (is.null(df)) {
      return(NULL)
    }
    replay_steps(df, steps())
  })

  shiny::observeEvent(input$add_step, {
    df <- cleaned_data()
    if (is.null(df)) {
      shiny::showNotification("Upload a CSV file first.", type = "warning")
      return()
    }

    action <- input$action
    col <- input$column
    if (identical(action, "none")) {
      return()
    }

    step <- switch(
      action,
      trim_whitespace = list(action = "trim_whitespace", col = col),
      upper = list(action = "change_case", col = col, case = "upper"),
      lower = list(action = "change_case", col = col, case = "lower"),
      mean = list(action = "impute", col = col, method = "mean"),
      median = list(action = "impute", col = col, method = "median"),
      scale_numeric = list(action = "scale_numeric", col = col),
      remove_duplicates = list(action = "remove_duplicates", col = NA_character_)
    )

    pipe <- pipeline()
    pipe$add_step(step)
    pipeline(pipe)
    steps(pipe$steps)
  })

  shiny::observeEvent(input$clear_steps, {
    df <- origin_df()
    if (is.null(df)) {
      return()
    }
    pipeline(Pipeline$new(df))
    steps(list())
  })

  output$preview <- shiny::renderTable({
    df <- cleaned_data()
    if (is.null(df)) {
      return(data.frame(Message = "Upload a CSV file to begin."))
    }
    utils::head(df, 25)
  })

  output$steps <- shiny::renderUI({
    current_steps <- steps()
    if (length(current_steps) == 0) {
      return(htmltools::tags$p("No steps applied yet."))
    }
    htmltools::tags$ol(lapply(current_steps, function(step) {
      htmltools::tags$li(step_label(step))
    }))
  })

  output$distribution <- shiny::renderPlot({
    df <- cleaned_data()
    col <- input$column
    if (is.null(df) || is.null(col) || !(col %in% names(df))) {
      return(NULL)
    }

    values <- stats::na.omit(df[[col]])
    if (length(values) == 0) {
      return(NULL)
    }

    plot_df <- data.frame(value = values)
    if (is.numeric(values)) {
      ggplot2::ggplot(plot_df, ggplot2::aes(x = value)) +
        ggplot2::geom_histogram(fill = "#0ea5e9", color = "white", bins = 30) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = col, y = "Count")
    } else {
      counts <- as.data.frame(table(value = values), stringsAsFactors = FALSE)
      counts <- utils::head(counts[order(-counts$Freq), ], 15)
      ggplot2::ggplot(counts, ggplot2::aes(x = stats::reorder(value, -Freq), y = Freq)) +
        ggplot2::geom_col(fill = "#0ea5e9") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(x = col, y = "Count")
    }
  })

  output$download_clean <- shiny::downloadHandler(
    filename = function() paste0("cleaned_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) utils::write.csv(cleaned_data(), file, row.names = FALSE)
  )

  invisible(NULL)
}
