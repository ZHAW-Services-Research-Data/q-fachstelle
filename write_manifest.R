# -----------------------------------------------------------------------------
# write_manifest.R
#
# Generates manifest.json for deployment to Posit Connect / shinyapps.io.
#
# Run this script from the project root whenever you:
#   - Add or remove packages
#   - Add or remove files
#   - Update package versions
#
# From the terminal:
#   Rscript write_manifest.R
#
# From the R console:
#   source("write_manifest.R")
# -----------------------------------------------------------------------------

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}

rsconnect::writeManifest(
  appDir   = getwd(),
  appFiles = c(
    "app.R",
    "dashboard_server.R",
    "dashboard_ui.R",
    "helpers.R",
    "data_processing.R",
    "get_redcap_data.R",
    "template/layout.R",
    "config.yml",
    "www/logo-negativ.png"
  )
)

message("manifest.json written successfully.")