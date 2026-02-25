# -----------------------------------------------------------------------------
# Dashboard UI -- *TO BE EDITED*
#
# Defines UI and dashboard content.
# -----------------------------------------------------------------------------

#' Dashboard UI
#'
#' Defines the UI for the dashboard content area. This function should return a
#' Shiny UI object (e.g., a tagList or a div) that contains
#' application-specific UI elements.
#'
#' @return A Shiny UI object representing the dashboard content.
dashboard_ui <- function() {
  tagList(
    
    # Row 1: Input for KITA code and lookup button
    fluidRow(
      column(
        width = 4,
        textInput(
          inputId = "inst_code",
          label = "Bitte geben Sie Ihren KITA-Umfragecode ein:",
        ),
        actionButton(
          inputId = "lookup_btn",
          label = "Resultate anzeigen",
          icon = icon("search")
        )
      )
    ),

    # Row 2: Placeholders for results and overall statistics
    fluidRow(
      column(
        width = 12,

        uiOutput("result_heading"),
        uiOutput("result_caption"),
        tableOutput(outputId = "result_table"),

        uiOutput("overall_heading"),
        uiOutput("overall_caption"),

        tableOutput(outputId = "overall_table")
      )
    )
  )
}