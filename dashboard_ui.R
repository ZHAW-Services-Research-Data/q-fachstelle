# -----------------------------------------------------------------------------
# Dashboard UI -- *TO BE EDITED*
#
# Defines UI and dashboard content.
#
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

    fluidRow(
      column(
        width = 4,
        textInput(
          inputId = "inst_code",
          label   = "Bitte geben Sie hier Ihren KITA‑Code ein:",
        ),
        actionButton(
          inputId = "lookup_btn",
          label   = "Resultate anzeigen",
          icon    = icon("search")
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        h3("Ergebnis‑Tabelle"),
        # No sanitize argument here – it belongs to renderTable()
        tableOutput(outputId = "result_table")
      )
    )
  )
}