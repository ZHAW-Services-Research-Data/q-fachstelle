library(stringr)
library(dplyr)

# Load helper that fetches REDCap data
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

# Normalise the `code` column → `code_base`
clean_data <- clean_data %>%
  mutate(
    code_base = code |>
      str_remove_all("\\s+") |>
      str_to_lower() |>
      str_remove_all("20\\d{2}") |>
      str_remove_all("\\.?\\d{2}\\b") |>
      str_replace_all("[\\._-]+$", "")
  )

# Example: focus on a specific code_base
current_code_base <- "kinderhausimago"

subset_data <- clean_data %>%
  filter(code_base == current_code_base) %>%
  select(all_of(c(relevant_vars, relevant_vars_quality)))

# Summarise by year: count rows and compute mean / SD for each quality metric
summary_by_year <- subset_data %>%
  group_by(year) %>%
  summarise(
    n = n(),
    across(
      all_of(relevant_vars_quality),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x,   na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# Show the result (optional)
print(summary_by_year)
