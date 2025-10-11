packages <- c("tidyr", "terra", "dplyr", "openxlsx")
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
S2_files[2] # 2020-04
S1_files[2] # 2020-03
S1_dates[2] <- "2020-04" # Wert manuel setzen, da S1 Bild vom 31.03.2020 stammt

# S2 Werte extrahieren
S2_values <- vect()
val_points <- data.frame()
for (i in seq_along(S2_files)) {
  cat(sprintf("\rExtracting Sentinel-2 values - Fortschritt: %d von %d (%s)", i, length(S2_files), basename(S2_files[i])))
  date <- format(as.Date(substr(basename(S2_files[i]), 12, 19), format = "%Y%m%d"), "%Y-%m")
  values <- terra::extract(rast(S2_files[i]), ref_data[ref_data$date == date, ], na.rm = TRUE, bind = TRUE)

  S2_values <- rbind(S2_values, values)
  val_points <- rbind(
    val_points,
    data.frame(
      date = date,
      points = nrow(values[which(!is.na(values$B02)), ]),
      soil = nrow(values[which(!is.na(values$B02) & values$class == 1), ]),
      residue = nrow(values[which(!is.na(values$B02) & values$class == 2), ]),
      vegetation = nrow(values[which(!is.na(values$B02) & values$class == 3), ])
    )
  )
  if (i == length(S2_files)) {
    cat("\rSentinel-2 Value extraction finished!                                                        \n")
  }
}
# save val_points
val_points$res_veg <- val_points$residue + val_points$vegetation
write.csv(val_points, "data/tables/S2_points_count_per_date.csv", row.names = FALSE)
write.xlsx(val_points, "data/tables/S2_points_count_per_date.xlsx", rowNames = FALSE)
View(val_points)
# extracted_values
ref_buf <- terra::buffer(ref_data, width = 50)
extracted_values <- vect()
# S1 Werte extrahieren
for (i in seq_along(S1_files)) {
  cat(sprintf("\rExtracting Sentinel-1 values - Fortschritt: %d von %d (%s)", i, length(S1_files), basename(S1_files[i])))
  date <- format(as.Date(substr(basename(S1_files[i]), 18, 25), format = "%Y%m%d"), "%Y-%m")
  date <- ifelse(date == "2020-03", "2020-04", date) # Wert manuel setzen, da S1-referenzbild vom 31.03.2020 stammt
  values <- terra::extract(rast(S1_files[i]), S2_values[S2_values$date == date, ], na.rm = TRUE, bind = TRUE)
  values_buff <- terra::extract(rast(S1_files[i])[[c("VH", "VV")]], ref_buf[ref_buf$date == date, ], na.rm = TRUE, bind = FALSE, fun = mean)
  names(values_buff) <- paste0(names(values_buff), "_buff")
  values <- cbind(values, values_buff[,-1])
  extracted_values <- rbind(extracted_values, values)

  if (i == length(S1_files)) {
    cat("\rSentinel-1 Value extraction finished!                                                        \n")
  }
}
# calculate indices
values(extracted_values) <- as.data.frame(extracted_values) %>%
  mutate(#row_id = row_number(),
         RVI = (4 * VH) / (VH + VV),
         RVI_buff = (4 * VH_buff) / (VH_buff + VV_buff),
         BSI = ((B11 + B04) - (B08 + B02)) / ((B11 + B04) + (B08 + B02)),
         NBR2 = (B11 - B12) / (B11 + B12),
         MSAVI2 = (2 * B08 + 1 - sqrt((2 * B08 + 1) ^ 2 - 8 * (B08 - B04))) / 2,
         BSI8A = ((B11 + B04) - (B8A + B02)) / ((B11 + B04) + (B8A + B02)),
         MSAVI8A = (2 * B8A + 1 - sqrt((2 * B8A + 1) ^ 2 - 8 * (B8A - B04))) / 2
  )
head(extracted_values)
# SpatVector speichern
writeVector(extracted_values, "Data/tables/extracted_values.gpkg", overwrite = TRUE)
# Tabelle speichern
write.csv(as.data.frame(extracted_values), "Data/tables/extracted_values.csv", row.names = FALSE)

# update metadata
extracted_values <- read.csv("Data/tables/extracted_values.csv", header = TRUE, stringsAsFactors = FALSE)
# wie viele Punkte pro Bodentyp
count <- as.data.frame(extracted_values)[complete.cases(extracted_values), ] %>%
  group_by(BODTYP_K) %>%
  summarise(aftr_masking = n()) %>%
  arrange(desc(aftr_masking))
soiltypes_aggr <- read.csv("data/tables/soiltypes_aggregated.csv")
soiltypes_aggr <- merge(soiltypes_aggr, count, by = "BODTYP_K", all.x = TRUE)
write.csv(soiltypes_aggr, "data/tables/soiltypes_aggregated.csv", row.names = FALSE)
View(soiltypes_aggr)
View(extracted_values)

points <- read.csv("data/tables/S2_points_count_per_date.csv", header = TRUE, stringsAsFactors = FALSE)
View(points)
