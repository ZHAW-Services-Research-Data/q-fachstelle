library(stringr)
library(dplyr)
library(fmsb)
library(tidyr)

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
  Bildung                                    = "bildung_qual_mean",
  `Interaktion und Kommunikation`            = "inter_kommun_qual_mean",
  `Inklusion, Integration und Partizipation` = "inkl_integ_parti_qual_mean",
  `Eltern- und Familienzusammenarbeit`       = "elternzusammenarbeit_qual_mean"
)

RADAR_VARNAMES <- c(
  "Bildung",
  "Interaktion und\nKommunikation",
  "Inklusion, Integration\nund Partizipation",
  "Eltern- und\nFamilien-\nzusammenarbeit"
)

QUALITY_COLS <- c(
  "Bildung",
  "Interaktion und Kommunikation",
  "Inklusion, Integration und Partizipation",
  "Eltern- und Familienzusammenarbeit"
)

COL_BENCHMARK <- "#DD8452"
COL_INST <- "#8172B2"


# Data loading & cleaning -------------------------------------------------

raw <- get_redcap_data()

data <- raw |>
  filter(form_1_complete == "2") |>
  filter(!code %in% EXCLUDE_CODES) |>
  mutate(
    year       = str_extract(datum, "20\\d{2}"),
    code_clean = normalize_code(code)
  ) |>
  select(all_of(c(COLS_ID, COLS_QUALITY)))


# Summaries ---------------------------------------------------------------

## Institution -------------------------------------------------------------

inst_code <- "kinderhausimago"
inst_clean <- normalize_code(inst_code)

inst_data <- data |> filter(code_clean == inst_clean)
inst_summary <- summarise_by_year(inst_data)

## Benchmark (all KITAs, same years) ---------------------------------------

relevant_years <- inst_summary$Jahr
n_plots <- length(relevant_years)

benchmark_summary <- data |>
  filter(year %in% relevant_years) |>
  summarise_by_year()


# Radar plots -------------------------------------------------------------

par(mfrow = c(1, n_plots), mar = c(2, 2, 3, 2))

for (yr in relevant_years) {
  bench_row <- benchmark_summary |>
    filter(Jahr == yr) |>
    select(all_of(QUALITY_COLS))
  inst_row <- inst_summary |>
    filter(Jahr == yr) |>
    select(all_of(QUALITY_COLS))

  radar_df <- rbind(
    setNames(as.data.frame(matrix(5, nrow = 1, ncol = 4)), QUALITY_COLS), # max
    setNames(as.data.frame(matrix(1, nrow = 1, ncol = 4)), QUALITY_COLS), # min
    bench_row,
    inst_row
  )

  radarchart(
    radar_df,
    axistype   = 1, # label style: 1 = labels on first axis only
    seg        = 4, # number of gridline rings (1-5 scale = 4 gaps)
    vlabels    = RADAR_VARNAMES, # axis label text
    vlcex      = 0.85, # axis label font size
    pcol       = c(COL_INST, COL_BENCHMARK), # polygon border colors
    pfcol      = adjustcolor(c(COL_INST, COL_BENCHMARK), alpha.f = 0.25), # polygon fill (transparent)
    plwd       = 2, # polygon border line width
    cglcol     = "grey70", # grid line color
    cglty      = 1, # grid line type (1 = solid)
    axislabcol = "grey40", # axis scale label color
    title      = paste("Qualitätsbewertung", yr) # plot title
  )

  legend(
    x      = "bottomleft", # legend position
    legend = c("Alle KITAs", "Diese KITA"), # legend labels
    col    = c(COL_BENCHMARK, COL_INST), # line colors matching the polygons
    lwd    = 2, # line width in legend
    bty    = "n", # no box around legend
    cex    = 0.85 # legend font size
  )
}

par(mfrow = c(1, 1))
