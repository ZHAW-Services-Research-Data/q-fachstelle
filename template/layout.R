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
#' @param links Optional list of named lists with 'label' and 'url' entries,
#'   rendered as navigation links on the right side of the header.
#'
#' @return A <header> tag element.
corporate_header <- function(title, logo, links = NULL) {
  nav_links <- if (!is.null(links)) {
    tags$nav(
      class = "app-header-nav",
      lapply(links, function(link) {
        tags$a(href = link$url, link$label, target = "_blank")
      })
    )
  }

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
    ),
    nav_links
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
#' Creates the footer section. Footer color is controlled by the brand CSS
#' class on the outer `.app-page` container (e.g. `.brand-black .app-footer`).
#'
#' @param ... UI elements to include inside the footer.
#'
#' @return A <footer> tag element.
corporate_footer <- function(...) {
  tags$footer(
    class = "app-footer",
    ...
  )
}


# Finds the first email address in a string and wraps it in a mailto link.
# Returns a tagList with the surrounding text preserved, or plain text if no
# email is found.
linkify_email <- function(text) {
  m <- regexpr("[a-zA-Z0-9._%+\\-]+@[a-zA-Z0-9.\\-]+\\.[a-zA-Z]{2,}", text, perl = TRUE)
  if (m == -1) return(text)
  start <- as.integer(m)
  end   <- start + attr(m, "match.length") - 1
  email <- substr(text, start, end)
  tagList(
    substr(text, 1, start - 1),
    tags$a(href = paste0("mailto:", email), email),
    substr(text, end + 1, nchar(text))
  )
}


#' Render Legal Notice
#'
#' Renders legal notice from a named config list:
#'   - institution: first line, always shown, bold
#'   - unit:        second line (label + optional link), hidden if label is blank
#'   - department, institute, contact: joined by " · " on the third line;
#'     any email address in contact is automatically made into a mailto link
#'
#' @param cfg Named list with keys: institution, unit, department, institute, contact.
#'
#' @return A tagList of <p> elements.
render_legal_notice <- function(cfg) {
  lines <- list()

  if (!is.null(cfg$institution) && nzchar(cfg$institution))
    lines <- c(lines, list(tags$p(tags$strong(cfg$institution))))

  if (!is.null(cfg$unit$label) && nzchar(cfg$unit$label)) {
    has_link <- !is.null(cfg$unit$link_url) && nzchar(cfg$unit$link_url) &&
                !is.null(cfg$unit$link_text) && nzchar(cfg$unit$link_text)
    unit_el <- if (has_link)
      tagList(cfg$unit$label, " · ",
        tags$a(href = cfg$unit$link_url, cfg$unit$link_text, target = "_blank"))
    else
      cfg$unit$label
    lines <- c(lines, list(tags$p(unit_el)))
  }

  # Build third line item by item so contact can contain a linked email
  parts <- list()
  for (key in c("department", "institute", "contact")) {
    val <- cfg[[key]]
    if (!is.null(val) && nzchar(val))
      parts <- c(parts, list(if (key == "contact") linkify_email(val) else val))
  }
  if (length(parts) > 0) {
    interleaved <- list()
    for (i in seq_along(parts)) {
      interleaved <- c(interleaved, list(parts[[i]]))
      if (i < length(parts)) interleaved <- c(interleaved, list(" · "))
    }
    lines <- c(lines, list(tags$p(do.call(tagList, interleaved))))
  }

  do.call(tagList, lines)
}
