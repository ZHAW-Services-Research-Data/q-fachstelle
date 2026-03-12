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
library(fmsb)

source("get_redcap_data.R")
source("helpers.R")


# Constants ---------------------------------------------------------------

EXCLUDE_CODES <- c("Rock", "Paper", "Scissors")

COLS_ID <- c(
  "code",
  "code_clean",
  "datum",
  "year",
  "form_1_complete"
)

COLS_QUALITY <- c(
  "bildung_qual",
  "inter_kommun_qual",
  "inkl_integ_parti_qual",
  "elternzusammenarbeit_qual"
)

RENAME_QUALITY_MEANS <- c(
  Bildung = "bildung_qual_mean",
  `Interaktion und Kommunikation` = "inter_kommun_qual_mean",
  `Inklusion, Integration und Partizipation` = "inkl_integ_parti_qual_mean",
  `Eltern- und Familienzusammenarbeit` = "elternzusammenarbeit_qual_mean"
)

QUALITY_COLS <- c(
  "Bildung",
  "Interaktion und Kommunikation",
  "Inklusion, Integration und Partizipation",
  "Eltern- und Familienzusammenarbeit"
)

RADAR_VARNAMES <- c(
  "Bildung",
  "Interaktion und\nKommunikation",
  "Inklusion, Integration\nund Partizipation",
  "Eltern- und\nFamilien-\nzusammenarbeit"
)

COL_BENCHMARK <- "#DD8452"
COL_INST <- "#8172B2"


# Server ------------------------------------------------------------------

#' Dashboard Server
#'
#' Defines server-side logic for the dashboard. Reacts to the lookup button,
#' filters data for the selected institution, computes summaries, renders
#' tables, and draws radar plots.
#'
#' @param input   Shiny input object.
#' @param output  Shiny output object.
#' @param session Shiny session object.
#'
#' @return None. Registers reactive behaviour.
dashboard_server <- function(input, output, session) {
  observeEvent(input$lookup_btn, {
    # Fetch fresh data from REDCap on every lookup
    raw <- get_redcap_data()

    data <- raw |>
      filter(form_1_complete == "2") |>
      filter(!code %in% EXCLUDE_CODES) |>
      mutate(
        year       = str_extract(datum, "20\\d{2}"),
        code_clean = normalize_code(code)
      ) |>
      select(all_of(c(COLS_ID, COLS_QUALITY)))

    # Clean user input and filter for the selected institution
    inst_clean <- normalize_code(input$inst_code)

    inst_data <- data |> filter(code_clean == inst_clean)

    # If nothing matches: show error and clear all outputs
    if (nrow(inst_data) == 0) {
      showNotification(
        paste0("Kein Datensatz gefunden für den Code '", input$inst_code, "'."),
        type = "error", duration = 5
      )
      output$result_heading <- renderUI(NULL)
      output$result_caption <- renderUI(NULL)
      output$result_table <- renderTable(NULL)
      output$overall_heading <- renderUI(NULL)
      output$overall_caption <- renderUI(NULL)
      output$overall_table <- renderTable(NULL)
      output$radar_heading <- renderUI(NULL)
      output$radar_ui <- renderUI(NULL)
      return()
    }


    # Summaries -----------------------------------------------------------

    inst_summary <- summarise_by_year(inst_data)

    relevant_years <- inst_summary$Jahr
    n_plots <- length(relevant_years)

    benchmark_summary <- data |>
      filter(year %in% relevant_years) |>
      summarise_by_year()


    # Tables --------------------------------------------------------------

    output$result_heading <- renderUI(h3("Ihre Ergebnisse"))
    output$result_caption <- renderUI(tags$div(
      tags$p("Die Tabelle zeigt die Mittelwerte der Selbsteinschätzungen für diesen KITA-Code sowie die Durchschnittswerte aller KITAs im gleichen Erhebungsjahr. Die Werte basieren auf einer Skala von 1 bis 5:"),
      tags$p(tags$strong("1"), " bedeutet, dass in diesem Bereich ein Entwicklungsbedarf erkannt wurde – der Bereich wird bisher wenig berücksichtigt oder es besteht Potenzial zur Weiterentwicklung."),
      tags$p(tags$strong("5"), " bedeutet, dass auf diesen Bereich bereits grosser Wert gelegt wird – die pädagogische Qualität in diesem Bereich ist gut ausgeprägt."),
      tags$p("Werte dazwischen weisen auf eine teilweise Umsetzung hin. Ein Vergleich mit dem KITA-Durchschnitt gibt Hinweise darauf, wo die eigene Einrichtung im Verhältnis zu anderen steht."),
      tags$br()
    ))
    output$result_table <- renderTable(
      inst_summary,
      sanitize.text.function = function(x) x
    )

    output$overall_heading <- renderUI(h3("Durchschnittswerte aller erfassten KITAs"))
    output$overall_caption <- renderUI(tags$div(
      tags$p("Diese Tabelle fasst die Mittelwerte aller KITAs zusammen, die im selben Jahr bzw. denselben Jahren Daten haben."),
      tags$br()
    ))
    output$overall_table <- renderTable(
      benchmark_summary,
      sanitize.text.function = function(x) x
    )


    # Radar plots ---------------------------------------------------------

    output$radar_heading <- renderUI(h3("Ihr Qualitätsprofil"))
    output$radar_ui <- renderUI({
      if (n_plots == 0) {
        return(NULL)
      }
      plot_outputs <- lapply(seq_len(n_plots), function(i) {
        div(
          style = "display: inline-block; vertical-align: top;",
          plotOutput(paste0("radar_plot_", i), width = "600px", height = "600px")
        )
      })
      do.call(tagList, plot_outputs)
    })

    for (i in seq_len(n_plots)) {
      local({
        yr <- relevant_years[i]

        output[[paste0("radar_plot_", i)]] <- renderPlot(
          {
            bench_row <- benchmark_summary |>
              filter(Jahr == yr) |>
              select(all_of(QUALITY_COLS))
            inst_row <- inst_summary |>
              filter(Jahr == yr) |>
              select(all_of(QUALITY_COLS))

            # fmsb requires the first two rows to be the max and min values
            radar_df <- rbind(
              setNames(as.data.frame(matrix(5, nrow = 1, ncol = 4)), QUALITY_COLS), # max
              setNames(as.data.frame(matrix(1, nrow = 1, ncol = 4)), QUALITY_COLS), # min
              inst_row,
              bench_row
            )

            radarchart(
              radar_df,
              axistype   = 1, # label style: 1 = labels on first axis only
              seg        = 4, # number of gridline rings (1-5 scale = 4 gaps)
              vlabels    = RADAR_VARNAMES, # axis label text
              vlcex      = 0.75, # axis label font size
              pcol       = c(COL_INST, COL_BENCHMARK), # polygon border colours
              pfcol      = adjustcolor(c(COL_INST, COL_BENCHMARK), alpha.f = 0.25), # polygon fill (transparent)
              plwd       = 2, # polygon border line width
              cglcol     = "grey70", # grid line colour
              cglty      = 1, # grid line type (1 = solid)
              axislabcol = "grey40", # axis scale label colour
              title      = paste("Qualitätsbewertung", yr) # plot title
            )

            legend(
              x      = "bottomleft", # legend position
              legend = c("Alle KITAs", "Diese KITA"), # legend labels
              col    = c(COL_BENCHMARK, COL_INST), # line colours matching the polygons
              lwd    = 2, # line width in legend
              bty    = "n", # no box around legend
              cex    = 0.85 # legend font size
            )
          },
          res = 96
        )
      })
    }
  })
}
