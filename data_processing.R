library(fmsb)
library(httr)
library(yaml)
library(stringr)

source("get_redcap_data.R")

data <- get_redcap_data()

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

# Only keep data where data$code is not based on REDCap auto-fills and that are complete
data <- data[data$form_1_complete == "2", ]
data <- data[!data$code %in% c("Rock", "Paper", "Scissors"), ]

data$year <- str_extract(data$datum, "20\\d{2}")

# Remove whitspace, lowercasing, remove 4-digit and 2-digit years, and remove trailing punctuation from the code variable
data$code_base <- data$code |>
  str_remove_all("\\s+") |>
  str_to_lower() |>
  str_remove_all("20\\d{2}") |> 
  str_remove_all("\\.?\\d{2}\\b") |>
  str_replace_all("[\\._-]+$", "") 

data$code_base

# assume code is kinderhausimago
current_code_base <- "kinderhausimago"

# subset for current_code_code, keep only relevant variables, and only keep complete records
data_subset <- data[data$code_base == current_code_base, relevant_vars]

# Aggregate subset: get mean for 


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
secret_path <- file.path("config", "secrets.yml")
secrets <- yaml::read_yaml(secret_path)

token <- secrets$REDCAP_TOKEN
url <- "https://redcap.zhaw.ch/api/"

formData <- list(
  "token"=token,
  content='record',
  action='export',
  format='json',
  type='flat',
  csvDelimiter='',
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='false',
  returnFormat='json'
)

# --- GET DATA ---
response <- POST(url, body = formData, encode = "form")
result_complete <- content(response)

# Keep only data in result_complete where form_1_complete == 2, meaning complete
result_complete <- Filter(function(x) x$form_1_complete == "2", result_complete)

# --- YEAR INFO ---
current_year <- 2025
previous_year <- 2024

# --- INIT VECTORS ---
bildung_qual_current_year <- c()
inter_kommun_qual_current_year <- c()
iipq_current_year <- c()
elternzusammenarbeit_qual_current_year <- c()

bildung_qual_previous_year <- c()
inter_kommun_qual_previous_year <- c()
iipq_previous_year <- c()
elternzusammenarbeit_qual_previous_year <- c()

bildung_qual_current_year_overall <- c()
inter_kommun_qual_current_year_overall <- c()
iipq_current_year_overall <- c()
elternzusammenarbeit_qual_current_year_overall <- c()

# --- FILTER AND COLLECT DATA ---
# Iterate over all elements (records) in result_complete
for (i in seq_along(result_complete)) {
  
  # Extract the i-th record from the list
  record <- result_complete[[i]]
  
  # Check whether the record belongs to the selected institution
  if (record$code == inst_code) {
    
    # If the record date contains the current year
    if (grepl(current_year, record$datum)) {
      
      # Append the education quality score (current year) as an integer
      bildung_qual_current_year <- c(
        bildung_qual_current_year,
        as.integer(record$bildung_qual)
      )
      
      # Append the intercultural communication quality score (current year)
      inter_kommun_qual_current_year <- c(
        inter_kommun_qual_current_year,
        as.integer(record$inter_kommun_qual)
      )
      
      # Append the inclusion/integration/participation quality score (current year)
      iipq_current_year <- c(
        iipq_current_year,
        as.integer(record$inkl_integ_parti_qual)
      )
      
      # Append the parent collaboration quality score (current year)
      elternzusammenarbeit_qual_current_year <- c(
        elternzusammenarbeit_qual_current_year,
        as.integer(record$elternzusammenarbeit_qual)
      )
      
      # Otherwise, if the record date contains the previous year
    } else if (grepl(previous_year, record$datum)) {
      
      # Append the education quality score (previous year)
      bildung_qual_previous_year <- c(
        bildung_qual_previous_year,
        as.integer(record$bildung_qual)
      )
      
      # Append the intercultural communication quality score (previous year)
      inter_kommun_qual_previous_year <- c(
        inter_kommun_qual_previous_year,
        as.integer(record$inter_kommun_qual)
      )
      
      # Append the inclusion/integration/participation quality score (previous year)
      iipq_previous_year <- c(
        iipq_previous_year,
        as.integer(record$inkl_integ_parti_qual)
      )
      
      # Append the parent collaboration quality score (previous year)
      elternzusammenarbeit_qual_previous_year <- c(
        elternzusammenarbeit_qual_previous_year,
        as.integer(record$elternzusammenarbeit_qual)
      )
    }
  }
  
  # --- Overall (all Kitas, regardless of institution code) ---
  
  # Check whether the record date contains the current year
  if (grepl(current_year, record$datum)) {
    
    # Append the education quality score to the overall current-year vector
    bildung_qual_current_year_overall <- c(
      bildung_qual_current_year_overall,
      as.integer(record$bildung_qual)
    )
    
    # Append the intercultural communication quality score (overall, current year)
    inter_kommun_qual_current_year_overall <- c(
      inter_kommun_qual_current_year_overall,
      as.integer(record$inter_kommun_qual)
    )
    
    # Append the inclusion/integration/participation quality score (overall, current year)
    iipq_current_year_overall <- c(
      iipq_current_year_overall,
      as.integer(record$inkl_integ_parti_qual)
    )
    
    # Append the parent collaboration quality score (overall, current year)
    elternzusammenarbeit_qual_current_year_overall <- c(
      elternzusammenarbeit_qual_current_year_overall,
      as.integer(record$elternzusammenarbeit_qual)
    )
  }
}


# --- CREATE DATAFRAME WITH TEXT INFO ---
b_q_me <- paste("Mittelwert Ihrer Kita:", round(mean(bildung_qual_current_year, na.rm = TRUE),2))
b_q_mi <- paste("Mindestwert Ihrer Kita:", min(bildung_qual_current_year, na.rm = TRUE))
b_q_ma <- paste("Maximalwert Ihrer Kita:", max(bildung_qual_current_year, na.rm = TRUE))
b_q_mo <- paste("Mittelwert aller Kitas:", round(mean(bildung_qual_current_year_overall, na.rm = TRUE),2))

i_k_me <- paste("Mittelwert Ihrer Kita:", round(mean(inter_kommun_qual_current_year, na.rm = TRUE),2))
i_k_mi <- paste("Mindestwert Ihrer Kita:", min(inter_kommun_qual_current_year, na.rm = TRUE))
i_k_ma <- paste("Maximalwert Ihrer Kita:", max(inter_kommun_qual_current_year, na.rm = TRUE))
i_k_mo <- paste("Mittelwert aller Kitas:", round(mean(inter_kommun_qual_current_year_overall, na.rm = TRUE),2))

iipq_me <- paste("Mittelwert Ihrer Kita:", round(mean(iipq_current_year, na.rm = TRUE),2))
iipq_mi <- paste("Mindestwert Ihrer Kita:", min(iipq_current_year, na.rm = TRUE))
iipq_ma <- paste("Maximalwert Ihrer Kita:", max(iipq_current_year, na.rm = TRUE))
iipq_mo <- paste("Mittelwert aller Kitas:", round(mean(iipq_current_year_overall, na.rm = TRUE),2))

ez_me <- paste("Mittelwert Ihrer Kita:", round(mean(elternzusammenarbeit_qual_current_year, na.rm = TRUE),2))
ez_mi <- paste("Mindestwert Ihrer Kita:", min(elternzusammenarbeit_qual_current_year, na.rm = TRUE))
ez_ma <- paste("Maximalwert Ihrer Kita:", max(elternzusammenarbeit_qual_current_year, na.rm = TRUE))
ez_mo <- paste("Mittelwert aller Kitas:", round(mean(elternzusammenarbeit_qual_current_year_overall, na.rm = TRUE),2))

df <- data.frame(
  Jahr = c(current_year, previous_year),
  Bildung = c(paste(b_q_me, b_q_mi, b_q_ma, b_q_mo),
              paste("Mittelwert Ihrer Kita:", round(mean(bildung_qual_previous_year, na.rm = TRUE),2))),
  `Interaktion und Kommunikation` = c(paste(i_k_me, i_k_mi, i_k_ma, i_k_mo),
                                      paste("Mittelwert Ihrer Kita:", round(mean(inter_kommun_qual_previous_year, na.rm = TRUE),2))),
  `Inklusion, Integration und Partizipation` = c(paste(iipq_me, iipq_mi, iipq_ma, iipq_mo),
                                                 paste("Mittelwert Ihrer Kita:", round(mean(iipq_previous_year, na.rm = TRUE),2))),
  `Eltern- und Familienzusammenarbeit` = c(paste(ez_me, ez_mi, ez_ma, ez_mo),
                                           paste("Mittelwert Ihrer Kita:", round(mean(elternzusammenarbeit_qual_previous_year, na.rm = TRUE),2)))
)

print(df)

# --- SPIDER CHART ---
Plotdata <- data.frame(matrix(NA, nrow = 5, ncol=5))
rownames(Plotdata) <- c("Max.", "Min.", paste("Mittelwert Kita", current_year),
                        paste("Mittelwert Kita", previous_year),
                        paste("Mittelwert aller Kitas", current_year))
colnames(Plotdata) <- c('Jahr','Bildung','Interaktion und Kommunikation','Inklusion, Integration und Partizipation','Eltern- und Familienzusammenarbeit')

Plotdata$Jahr <- c(NA, NA, current_year, previous_year, current_year)
Plotdata$Bildung <- c(5, 0, mean(bildung_qual_current_year, na.rm = TRUE), mean(bildung_qual_previous_year, na.rm = TRUE), mean(bildung_qual_current_year_overall, na.rm = TRUE))
Plotdata$`Interaktion und Kommunikation` <- c(5, 0, mean(inter_kommun_qual_current_year, na.rm = TRUE), mean(inter_kommun_qual_previous_year, na.rm = TRUE), mean(inter_kommun_qual_current_year_overall, na.rm = TRUE))
Plotdata$`Inklusion, Integration und Partizipation` <- c(5, 0, mean(iipq_current_year, na.rm = TRUE), mean(iipq_previous_year, na.rm = TRUE), mean(iipq_current_year_overall, na.rm = TRUE))
Plotdata$`Eltern- und Familienzusammenarbeit` <- c(5, 0, mean(elternzusammenarbeit_qual_current_year, na.rm = TRUE), mean(elternzusammenarbeit_qual_previous_year, na.rm = TRUE), mean(elternzusammenarbeit_qual_current_year_overall, na.rm = TRUE))

areas <- c(rgb(0/255, 175/255, 187/255, 0.25),
           rgb(231/255, 184/255, 0/255, 0.25),
           rgb(252/255, 78/255, 7/255, 0.25))

par(mfrow=c(3, 1))
radarchart(Plotdata[c(1,2,3),2:5], pfcol=areas[1], pcol="#00AFBB", plwd=2, cglcol="gray", cglwd=1, cglty=1,
           axistype=1, caxislabels=1:5, calcex=2, axislabcol="black",
           title=paste("Mittelwert Kita", current_year), vlcex=2)

radarchart(Plotdata[c(1,2,4),2:5], pfcol=areas[2], pcol="#E7B800", plwd=2, cglcol="gray", cglwd=1, cglty=1,
           axistype=1, caxislabels=1:5, calcex=2, axislabcol="black",
           title=paste("Mittelwert Kita", previous_year), vlcex=2)

radarchart(Plotdata[c(1,2,5),2:5], pfcol=areas[3], pcol="#FC4E07", plwd=2, cglcol="gray", cglwd=1, cglty=1,
           axistype=1, caxislabels=1:5, calcex=2, axislabcol="black",
           title="Mittelwert aller Kitas", vlcex=2)
