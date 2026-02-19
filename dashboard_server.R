# -----------------------------------------------------------------------------
# Dashboard Server Logic -- *TO BE EDITED*
#
# Defines server-side logic for the dashboard.
#
# Load static datasets above the server function so they are read once
# when the app starts (recommended).
# -----------------------------------------------------------------------------
library(shiny)
library(dplyr)
library(stringr)
library(stringdist)
library(fmsb)

source("get_redcap_data.R")

#' Clean KITA Code
#' 
#' Cleans and normalizes KITA codes by:
#'  1. Trimming whitespace
#'  2. Converting to lower case.
#'  3. Remove any non-alphanumeric characters.
#'  4. Codes contain numbers of the current year, starting in 2023 to 2026 Ensure that 23 is converted to 2023, 24 to 2024, and so son
#'
#'  @param input A raw KITA code string.
#'  
#'  @return A cleaned, normalized KITA code string. 
clean_code <- function(x) {
  x <- str_trim(x)
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9_\\.]", "")
  x <- str_replace_all(x, "[_.]", "")
  x <- str_replace_all(
    x,
    "(?<!\\d)(23|24|25|26)(?!\\d)",
    "20\\1"
  )
  return(x)
}

data <- get_redcap_data()

data_clean <- data %>%
  filter(form_1_complete == "2") %>%
  filter(!code %in% c("Rock", "Paper", "Scissors")) %>%
  mutate(
    year = str_extract(datum, "20\\d{2}"),
    code_clean = clean_code(code)
)

# Variables we care about
relevant_vars <- c(
  "code",
  "code_clean",
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

# Only keep the relevant columns for the dashboard
data_clean <- data_clean %>% 
  select(all_of(c(relevant_vars, relevant_vars_quality)))


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
  
  observeEvent(input$lookup_btn, {
    
    # Clean the user entry and get the subset for that KITA
    user_clean <- clean_code(input$inst_code)
    
    subset_data <- data_clean %>%
      filter(code_clean == user_clean) %>%
      select(all_of(c(relevant_vars, relevant_vars_quality)))
    
    # If nothing matches --> error message & clear outputs
    if (nrow(subset_data) == 0) {
      showNotification(
        paste0("Kein Datensatz gefunden für den Code '", input$inst_code, "'."),
        type = "error", duration = 5
      )
      output$result_table   <- renderTable(NULL)
      output$result_heading <- renderUI(NULL)
      output$result_caption <- renderUI(NULL)
      
      # also clear the second table & its headings
      output$overall_table   <- renderTable(NULL)
      output$overall_heading <- renderUI(NULL)
      output$overall_caption <- renderUI(NULL)
      
      return()
    }
    
    # First summary -- only selected KITA
    summary_by_year <- subset_data %>%
      group_by(year) %>%
      summarise(
        n = n(),
        across(
          all_of(relevant_vars_quality),
          ~mean(.x, na.rm = TRUE),
          .names = "{.col}_mean"
        ),
        .groups = "drop"
      ) %>%
      rename(
        Jahr = year,
        `Anzahl Bewertungen` = n,
        Bildung = bildung_qual_mean,
        `Interaktion und Kommunikation` = inter_kommun_qual_mean,
        `Inklusion, Integration und Partizipation` = inkl_integ_parti_qual_mean,
        `Eltern- und Familienzusammenarbeit` = elternzusammenarbeit_qual_mean
      )
    
    # Get relevant years from summary
    relevant_years <- summary_by_year$Jahr
    
    # Second summary -- all KITAs for the same years
    summary_by_certain_years_all_Kitas <- data_clean %>%
      filter(year %in% relevant_years) %>%
      group_by(year) %>%
      summarise(
        n = n(),
        across(
          all_of(relevant_vars_quality),
          ~mean(.x, na.rm = TRUE),
          .names = "{.col}_mean"
        ),
        .groups = "drop"
      ) %>%
      rename(
        Jahr = year,
        `Anzahl Bewertungen` = n,
        Bildung = bildung_qual_mean,
        `Interaktion und Kommunikation` = inter_kommun_qual_mean,
        `Inklusion, Integration und Partizipation` = inkl_integ_parti_qual_mean,
        `Eltern- und Familienzusammenarbeit` = elternzusammenarbeit_qual_mean
      )
    
    # Render the heading & caption for the first table
    output$result_heading <- renderUI({
      h3(paste0("Ergebnis‑Tabelle für KITA mit Code ", input$inst_code))
    })
    output$result_caption <- renderUI({
      tags$div(
        "Die Tabelle zeigt die Mittelwerte der gefundenen Einträge für diesen Code."
      )
    })
    
    #  Render the first table (KITA‑specific)
    output$result_table <- renderTable({
      summary_by_year
    }, sanitize.text.function = function(x) x)
    
    # Render heading & caption for the second table
    output$overall_heading <- renderUI({
      h3("Durchschnittswerte aller KITAs für die gleichen Jahre")
    })
    output$overall_caption <- renderUI({
      tags$div(
        "Diese Tabelle fasst die Mittelwerte aller KITAs zusammen, die im selben Jahr bzw. denselben Jahren Daten haben."
      )
    })
    
    # Render the second table (overall averages)
    output$overall_table <- renderTable({
      summary_by_certain_years_all_Kitas
    }, sanitize.text.function = function(x) x)
    
  })
}