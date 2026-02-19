# -----------------------------------------------------------------------------
# Dashboard Server Logic -- *TO BE EDITED*
#
# Defines server-side logic for the dashboard.
#
# Load static datasets above the server function so they are read once
# when the app starts (recommended).
# -----------------------------------------------------------------------------

#' Clean KITA Code
#' 
#' Cleans and normalizes KITA codes by:
#'  1. Trimming whitespace and converting to lower case.
#'  2. Replacing non-alphanumeric characters with underscores.
#'  3. Removing year fragments (e.g. "_2023", ".23
#'  4. Collapsing consecutive underscores into a single underscore.
#'  5. Removing trailing underscores.
#'
#'  @param input A raw KITA code string.
#'  
#'  @return A cleaned, normalized KITA code string. 
clean_code <- function(x) {
  x <- str_trim(x)
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^[:alnum:]]+", "_")
  x <- str_remove_all(x, "(?:[_\\.])(?:20\\d{2}|\\d{2})\\b")
  x <- str_replace_all(x, "_+", "_")
  x <- str_remove(x, "_$")
  
  return(x)
}

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

source("get_redcap_data.R")

# Pull the raw dataset
raw_data <- get_redcap_data()

# Variables we care about
relevant_vars <- c(
  "code",
  "code_base",
  "datum",
  "year",
  "form_1_complete"
)

relevant_vars_quality <- c(
  "bildung_qual",
  "inter_kommun_qual",
  "inkl_integ_parti_qual",
  "elternzusammenarbeit_qual"
)

# Keep only completed forms (value "2") and drop auto‑filled codes
clean_data <- raw_data %>%
  filter(form_1_complete == "2") %>%
  filter(!code %in% c("Rock", "Paper", "Scissors"))

# Extract year from the datum column (expects a YYYY somewhere in the string)
clean_data <- clean_data %>%
  mutate(year = str_extract(datum, "20\\d{2}"))