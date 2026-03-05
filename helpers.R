#' Normalize an institution code for reliable matching
#'
#' Strips whitespace, converts to lower-case, removes all characters that are
#' not ASCII letters or digits, and expands two-digit year abbreviations
#' (23–26) to four-digit form (2023–2026).
#'
#' @param code `character` vector of raw institution codes.
#'
#' @return `character` vector of the same length as `code`, with each element
#'   normalized.
#'
#' @examples
#' normalize_code("Kinder.Haus_Imago 24")
#' #> [1] "kinderhausimago2024"
normalize_code <- function(code) {
  code |>
    str_trim() |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9_\\.]", "") |>
    str_replace_all("[_.]", "") |>
    str_replace_all("(?<!\\d)(2[3-6])(?!\\d)", "20\\1")
}

#' Summarise quality scores by year
#'
#' Groups a data frame by year and computes the mean of every quality column,
#' then renames the result columns to human-readable German labels.
#'
#' @param df `data.frame` that must contain the columns `year` and all entries
#'   of [COLS_QUALITY].
#'
#' @return A `tibble` with one row per year and the columns
#'   *Jahr*, *Anzahl Bewertungen*, and one labelled column per quality
#'   dimension.
summarise_by_year <- function(df) {
  df |>
    group_by(year) |>
    summarise(
      n = n(),
      across(all_of(COLS_QUALITY), \(x) mean(x, na.rm = TRUE), .names = "{.col}_mean"),
      .groups = "drop"
    ) |>
    rename(Jahr = year, `Anzahl Bewertungen` = n) |>
    rename(all_of(RENAME_QUALITY_MEANS))
}