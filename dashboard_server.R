# -----------------------------------------------------------------------------
# Dashboard Server Logic -- *TO BE EDITED*
#
# Defines server-side logic for the dashboard.
#
# Load static datasets above the server function so they are read once
# when the app starts (recommended).
# -----------------------------------------------------------------------------

# Example:
# data <- readRDS("data/data.rds")


#' Server
#'
#' Defines server-side logic for the dashboard.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return None. Registers reactive behavior.
dashboard_server <- function(input, output, session) {

  # Use reactive() only if data must update during runtime.

}
