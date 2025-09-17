packages <- c("tidyr","terra") #  "dplyr", "stringr", "xml2", 
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}

# Daten laden
S2_files <- list.files("Data/S2/pro/mosaic", pattern = "\\.tif$", full.names = TRUE)
S1_files <- list.files("Data/S1/pro/mosaic/mean/texture", pattern = "\\.tif$", full.names = TRUE)
ref_data <- terra::vect("data/ref_data_soil_merge_32632.gpkg")

# Zeitstempel extrahieren
S2_dates <- unique(format(as.Date(substr(basename(S2_files), 12, 19), format = "%Y%m%d"), "%Y-%m"))
S1_dates <- unique(format(as.Date(substr(basename(S1_files), 18, 25), format = "%Y%m%d"), "%Y-%m"))
S2_files[2]
S1_files[2]

# S2 Werte extrahieren
S2_values <- vect()

for (i in seq_along(S2_files)) {
  cat(sprintf("\rExtracting Sentinel-2 values - Fortschritt: %d von %d (%s)", i, length(S2_files), basename(S2_files[i])))
  date <- format(as.Date(substr(basename(S2_files[i]), 12, 19), format = "%Y%m%d"), "%Y-%m")
  values <- terra::extract(rast(S2_files[i]), ref_data[ref_data$date == date, ], na.rm = TRUE, bind = TRUE)

  S2_values <- rbind(S2_values, values)

  if (i == length(S2_files)) {
    cat("\rSentinel-2 Value extraction finished!                                                        \n")
  }
}

# extracted_values
extracted_values <- vect()
# S1 Werte extrahieren
for (i in seq_along(S1_files)) {
  cat(sprintf("\rExtracting Sentinel-1 values - Fortschritt: %d von %d (%s)", i, length(S1_files), basename(S1_files[i])))
  date <- format(as.Date(substr(basename(S1_files[i]), 18, 25), format = "%Y%m%d"), "%Y-%m")
  values <- terra::extract(rast(S1_files[i]), S2_values[S2_values$date == date, ], na.rm = TRUE, bind = TRUE)

  extracted_values <- rbind(extracted_values, values)

  if (i == length(S1_files)) {
    cat("\rSentinel-1 Value extraction finished!                                                        \n")
  }
}

# calculate indizes
values(extracted_values) <- as.data.frame(extracted_values) %>%
  mutate(#row_id = row_number(),
         RVI = (4 * VH) / (VH + VV),
         BSI = ((B11 + B04) - (B08 + B02)) / ((B11 + B04) + (B08 + B02)),
         NBR2 = (B11 - B12) / (B11 + B12))

head(extracted_values)
# SpatVector speichern
writeVector(extracted_values, "Data/tables/extracted_values.gpkg", overwrite = TRUE)
# Tabelle speichern
write.csv(as.data.frame(extracted_values), "Data/tables/extracted_values.csv", row.names = FALSE)

# update metadata
count <- as.data.frame(extracted_values) %>%
  group_by(BODTYP_K) %>%
  summarise(aftr_masking = n()) %>%
  arrange(desc(aftr_masking))
soiltypes_aggr <- read.csv("data/tables/soiltypes_aggregated.csv")
soiltypes_aggr <- merge(soiltypes_aggr, count, by = "BODTYP_K", all.x = TRUE)
write.csv(soiltypes_aggr, "data/tables/soiltypes_aggregated.csv", row.names = FALSE)

View(as.data.frame(extracted_values))


  values <- terra::extract(rast(S1_files[1]), ref_data[ref_data$date == "2018-04", ], na.rm = TRUE, bind = TRUE)
  df <- as.data.frame(values)

View(df)

hist(as.vector(rast(S1_files[1]), "VH"), breaks = 100)
plot(rast(S1_files[1]), "VH_mean")
plot(quantized)
plot(rast)
