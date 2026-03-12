# -----------------------------------------------------------------------------
# Main Application -- *DO NOT EDIT*
#
# Entry point for the ZHAW Shiny dashboard template:
#   - Load configuration
#   - Load layout helpers and dashboard content
#   - Assemble UI structure
#   - Initialize Shiny application
#
# Application-specific content should be defined in `dashboard_ui.R`.
# Application logic should be defined in `dashboard_server.R.
# -----------------------------------------------------------------------------


library(shiny)
library(yaml)

config <- yaml::read_yaml("config.yml")

# Validate config
if (is.null(config$title) || !nzchar(config$title)) {
  stop("config.yml: 'title' is missing or empty.")
}

source("template/layout.R")
source("dashboard_ui.R")
source("dashboard_server.R")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  div(
    class = "app-page",

    corporate_header(
      title = config$title,
      logo  = "logo-negativ.png",
      links = config$header_links
    ),

    corporate_content(
      dashboard_ui()
    ),

    corporate_footer(
      render_legal_notice(config$legal_notice)
    )
  )
)

shinyApp(ui, dashboard_server)
