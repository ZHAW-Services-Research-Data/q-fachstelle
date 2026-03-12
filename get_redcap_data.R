# ------------------------------------------------------------------------------
# get_redcap_data.R
#
# Downloads records from REDCap and return them as a data frame.
#
# The section labeled "API Playground Code" is copied from the REDCap
# API Playground and should be copy+pasted from there and edited if export
# options change.
# ------------------------------------------------------------------------------

get_redcap_data <- function(secret_path = file.path("config", "secrets.yml")) {
  library(httr)
  library(yaml)

  secrets <- yaml::read_yaml(secret_path)
  token <- secrets$REDCAP_TOKEN
  url <- secrets$REDCAP_URL

  stopifnot(!is.null(token), !is.null(url))

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>
  # REDCap API Playground Code
  # <<<<<<<<<<<<<<<<<<<<<<<<<<

  formData <- list(
    "token" = token,
    content = "record",
    action = "export",
    format = "csv",
    type = "flat",
    csvDelimiter = "",
    rawOrLabel = "raw",
    rawOrLabelHeaders = "raw",
    exportCheckboxLabel = "false",
    exportSurveyFields = "false",
    exportDataAccessGroups = "false",
    returnFormat = "json"
  )
  response <- httr::POST(url, body = formData, encode = "form")
  result <- httr::content(response)

  # <<<<<<<<<<<<>>>>>>>>>>>>>>>>>

  httr::stop_for_status(response)

  raw_csv <- httr::content(response, as = "text", encoding = "UTF-8")
  read.csv(text = raw_csv, stringsAsFactors = FALSE)
}

data <- get_redcap_data()
