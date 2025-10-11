# S1 Preprocessing Script
# This script preprocesses Sentinel-1 GRD data using SNAP's Graph Processing Tool (GPT).
# It includes steps for subsetting, border noise removal, calibration, terrain correction, and conversion to dB scale.
start_time <- Sys.time()
packages <- c("tidyr", "dplyr", "stringr", "xml2", "terra")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}
# ref_data laden
ref_data <- terra::vect("data/ref_data.gpkg") %>% project("EPSG:32632") # <- crs der Sentinel-2 Daten
# AOI laden
wkt <- readLines("data/tables/ref_data_extent_epsg32632_buff30m.wkt")
# available files:
zip_files <- list.files("Data/S2/RAW", pattern = "\\.zip$", full.names = TRUE)
#zip_files <- zip_files[1:2]

# for (i in seq_along(zip_files)) {

#   cat(sprintf("\rProcessing zip file %d of %d", i, length(zip_files)))
#   files_in_zip <- unzip(zip_files[i], list = TRUE)

#   target_files_10m <- files_in_zip$Name[grepl("B02_10m\\.jp2$|B03_10m\\.jp2$|B04_10m\\.jp2$|B08_10m\\.jp2$", files_in_zip$Name)]
#   target_files_20m <- files_in_zip$Name[grepl("B05_20m\\.jp2$|B06_20m\\.jp2$|B07_20m\\.jp2$|B8A_20m\\.jp2$|B11_20m\\.jp2$|B12_20m\\.jp2$", files_in_zip$Name)]

#   scl_file <- files_in_zip$Name[grepl("SCL_20m\\.jp2$", files_in_zip$Name)]

#   scene_1 <- rast(unzip(zip_files[i], files = target_files_10m)) %>% crop(wkt) 

#   scene_2 <- rast(unzip(zip_files[i], files = target_files_20m)) %>% resample(scene_1, method = "near") %>% crop(wkt)

#   scl <- rast(unzip(zip_files[i], files = scl_file)) %>% resample(scene_1, method = "near") %>% crop(wkt)

#   scene <- c(scene_1, scene_2)

#   masked <- mask(scene, scl, maskvalues = c(3,8,9,10))

#   writeRaster(masked, paste0("Data/S2/pro/", substr(basename(zip_files[i]), 1, 50), "_subset_masked.tif"), overwrite = TRUE)

#   if (i == length(zip_files)) {
#     cat("\rProcessing finished!                          \n")
#   }
# }

# buffer
for (i in seq_along(zip_files)) {

  cat(sprintf("\rProcessing zip file %d of %d", i, length(zip_files)))
  files_in_zip <- unzip(zip_files[i], list = TRUE)

  target_files_10m <- files_in_zip$Name[grepl("B02_10m\\.jp2$|B03_10m\\.jp2$|B04_10m\\.jp2$|B08_10m\\.jp2$", files_in_zip$Name)]
  target_files_20m <- files_in_zip$Name[grepl("B05_20m\\.jp2$|B06_20m\\.jp2$|B07_20m\\.jp2$|B8A_20m\\.jp2$|B11_20m\\.jp2$|B12_20m\\.jp2$", files_in_zip$Name)]

  scl_file <- files_in_zip$Name[grepl("SCL_20m\\.jp2$", files_in_zip$Name)]

  scene_1 <- rast(unzip(zip_files[i], files = target_files_10m, exdir = "Data/S2/RAW")) %>% crop(wkt) 
  scene_2 <- rast(unzip(zip_files[i], files = target_files_20m, exdir = "Data/S2/RAW")) %>% resample(scene_1, method = "near") %>% crop(wkt)
  scl <- rast(unzip(zip_files[i], files = scl_file, exdir = "Data/S2/RAW")) %>% resample(scene_1, method = "near") %>% crop(wkt)

  scene <- c(scene_1, scene_2)

  # Binäre Maske aus SCL erstellen (Werte 3,8,9,10 maskieren)
  mask_raster <- scl
  values(mask_raster) <- as.numeric(values(scl) %in% c(3, 8, 9, 10))

  # Runde Pufferung mit Radius 200 m
  w <- focalMat(mask_raster, 200, type = "circle", fillNA = TRUE)
  w[!is.na(w)] <- 1
  buffered_mask <- focal(mask_raster, w = w, fun = max, na.policy = "omit", fillvalue = 0)

  # Alle gepufferten Zellen maskieren (auf NA setzen)
  values(buffered_mask)[values(buffered_mask) == 1] <- NA

  # Maskieren mit gepufferter Maske
  masked <- mask(scene, buffered_mask)

  # Speichern
  writeRaster(masked, paste0("Data/S2/pro/", substr(basename(zip_files[i]), 1, 50), "_subset_masked.tif"), overwrite = TRUE)

  # entpackte Dateien löschen
  unlink(str_sub(zip_files[i], end = -5), recursive = TRUE)

  if (i == length(zip_files)) {
    cat("\rProcessing finished!                                              \n")
  }
}

tif_files <- list.files("Data/S2/pro", pattern = "\\.tif$", full.names = TRUE)
#tif_files <- tif_files[1:2]
# create dataframe with file names and dates
df <- data.frame(file = basename(tif_files), date = as.Date(substr(basename(tif_files), 12, 19), format = "%Y%m%d"))

# find duplicates and mosaic them
unique_dates <- unique(df$date)

for (i in 1:length(unique_dates)) {
  current_date <- unique_dates[i]
  cat(sprintf("\rMosaicing - Fortschritt: %d von %d (%s)", i, length(unique_dates), unique_dates[i]))
  if (sum(df$date == current_date) > 1) {
    index <- which(df$date == current_date)
    # mosaicing
    mosaic <- mosaic(rast(tif_files[index[1]]), rast(tif_files[index[2]]), fun = "mean")

    names(mosaic) <- gsub(".*(B[0-9]{1,2}[A-Z]?).*", "\\1", names(mosaic), perl = TRUE)

    writeRaster(
      mosaic,
      filename = paste0("Data/S2/pro/mosaic/", str_sub(basename(tif_files[index[1]]), end = -42), "mosaic.tif"),
      overwrite = TRUE
    )
  }
  if (i == length(unique_dates)) {
    cat("\rMosaicing finished!                                              \n")
  }
}

# actuallise tif_files
# tif_files <- list.files("Data/S2/pro/mosaic", pattern = "\\.tif$", full.names = TRUE)
# #tif_files <- tif_files[1]
# names(rast(tif_files[1]))
# # RGB Plotten
# for (i in seq_along(tif_files)) {
#   cat(sprintf("\rPlotting - Fortschritt: %d von %d (%s)", i, length(tif_files), basename(tif_files[i])))

#   img <- rast(tif_files[i])

#   # als PNG speichern
#   png(width = ncol(img), height = nrow(img), file = paste0("Data/plots/", str_sub(basename(tif_files[i]), end = -11), "plot.png"), units = "px", res = 300)
#   plotRGB(img, r = 3, g = 2, b = 1, stretch = "lin")
#   dev.off()

#   if (i == length(tif_files)) {
#     cat("\rPlotting finished!                                                      \n")
#   }
# }
# extracted_values <- vect()

# # Werte extrahieren
# for (i in seq_along(tif_files)) {
#   cat(sprintf("\rExtracting values - Fortschritt: %d von %d (%s)", i, length(tif_files), basename(tif_files[i])))
#   date <- format(as.Date(substr(basename(tif_files[i]), 12, 19), format = "%Y%m%d"), "%Y-%m")
#   values <- terra::extract(rast(tif_files[i]), ref_data[ref_data$date == date, ], na.rm = TRUE, bind = TRUE)

#   extracted_values <- rbind(extracted_values, values)

#   if (i == length(tif_files)) {
#     cat("\rValue extraction finished!                                                        \n")
#   }
# }

# end_time <- Sys.time()
# cat(sprintf("Total processing time: %s\n", difftime(end_time, start_time, units = "mins")))