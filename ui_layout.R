# -----------------------------------------------------------------------------
# UI Layout Helpers -- *DO NOT EDIT*
#
# Defines structural UI components for the ZHAW Shiny dashboard template:
#   - Header
#   - Content wrapper
#   - Footer
#   - Legal notice renderer
#
# This file contains layout logic only.
# No application-specific content should be added here.
# -----------------------------------------------------------------------------


#' Corporate Header
#'
#' Creates the fixed header section of the dashboard.
#'
#' @param title Character string. Dashboard title displayed in the header.
#' @param logo Character string. Filename of the logo located in /www.
#'
#' @return A <header> tag element.
corporate_header <- function(title, logo) {
  tags$header(
    class = "app-header",
    tags$img(
      src = logo,
      class = "app-logo",
      alt = "Institution logo"
    ),
    tags$h1(
      title,
      class = "app-title"
    )
  )
}

#' Corporate Content Wrapper
#'
#' Wraps dashboard content in a standardized layout container.
#'
#' @param ... UI elements to include in the content area.
#'
#' @return A <div> tag element with class "app-content".
corporate_content <- function(...) {
  div(
    class = "app-content",
    ...
  )
}

#' Corporate Footer
#'
#' Creates the footer section. The footer color depends on the selected brand.
#'
#' @param brand Character string. Either "standard" or "black".
#' @param ... UI elements to include inside the footer.
#'
#' @return A <footer> tag element.
corporate_footer <- function(brand, ...) {
  footer_style <- if (brand == "black") {
    "color: #000000;"
  } else {
    NULL # keep CSS default (blue)
  }

  tags$footer(
    class = "app-footer",
    style = footer_style,
    ...
  )
}


#' Render Legal Notice
#'
#' Renders legal notice items as a single line separated by dots.
#'
#' @param lines Character vector containing legal notice entries.
#'
#' @return A <p> tag element.
render_legal_notice <- function(lines) {
  tags$p(
    paste(lines, collapse = " · ")
  )
}
