# calculate statistics for extracted values
packages <- c("lsr", "openxlsx")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}

# Daten laden
data <- read.csv("Data/tables/extracted_values.csv", header = TRUE, stringsAsFactors = FALSE)
str(data)

# Cohens d berechnen
## pro Zeitpunkt
dates <- unique(data$date)
variables <- names(data)[-c(1, 2, 3)]
  cohens_D <- data.frame(
    date = as.Date(character()),
    variable = character(),
    soil_res = numeric(),
    soil_veg = numeric(),
    res_veg = numeric(),
    stringsAsFactors = FALSE
  )
for (date in dates) {
  data_sub <- data[data$date == date, ]
  for (var in variables) {
    date <- date
    soil_res <- cohensD(data_sub[[var]][data_sub$class == 1], data_sub[[var]][data_sub$class == 2])
    soil_veg <- cohensD(data_sub[[var]][data_sub$class == 1], data_sub[[var]][data_sub$class == 3])
    res_veg <- cohensD(data_sub[[var]][data_sub$class == 2], data_sub[[var]][data_sub$class == 3])
    soil_other <- cohensD(data_sub[[var]][data_sub$class == 1], data_sub[[var]][data_sub$class == 2 | data_sub$class == 3])
    cohens_D <- rbind(cohens_D, data.frame(variable = var,
                                           date = date,
                                           soil_res = soil_res,
                                           soil_veg = soil_veg,
                                           res_veg = res_veg,
                                           soil_other = soil_other)
    )
  }
}
write.csv(cohens_D, paste0("Data/tables/cohens_d_per_date.csv"), row.names = FALSE)

## über alle Zeitpunkte
cohens_D <- data.frame(
  variable = character(),
  soil_res = numeric(),
  soil_veg = numeric(),
  soil_other = numeric(),
  res_veg = numeric(),
  stringsAsFactors = FALSE
)

for (var in variables) {
  soil_res <- cohensD(data[[var]][data$class == 1], data[[var]][data$class == 2])
  soil_veg <- cohensD(data[[var]][data$class == 1], data[[var]][data$class == 3])
  res_veg <- cohensD(data[[var]][data$class == 2], data[[var]][data$class == 3])
  soil_other <- cohensD(data[[var]][data$class == 1], data[[var]][data$class == 2 | data$class == 3])
  cohens_D <- rbind(cohens_D, data.frame(variable = var, soil_res = soil_res, soil_veg = soil_veg, residue_veg = res_veg, soil_other = soil_other))
}
cohens_D <- arrange(cohens_D, desc(soil_other))
write.csv(cohens_D, paste0("Data/tables/cohens_d_overall.csv"), row.names = FALSE)
write.xlsx(cohens_D, paste0("Data/tables/cohens_d_overall.xlsx"), rowNames = FALSE)

stop("manuelle auswahl der besten Variablen notwendig")
## best optical: NBR2, B8A, MSAVI8A, MSAVI2, BSI8A
## best SAR: RVI_buff, VH_mean, VH_variance
best_vars <- c("NBR2", "B8A", "MSAVI8A", "MSAVI2", "BSI8A", "RVI_buff", "VH_mean", "VH_variance")
writeLines(best_vars, "Data/tables/best_CohensD_performing_variables.txt")
