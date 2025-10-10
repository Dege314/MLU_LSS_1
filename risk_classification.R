packages <- c("randomForest", "terra", "dplyr", "ggplot2", "ggpubr", "sf", "splitTools", "tidyterra", "stringr")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}
# Klassifizierung der S1 und S2 Szenen mit RF
# nummern für Bodentypen vergeben
mapping <- c("BB-TT" = 1, "GGh" = 2, "LF" = 3, "LL" = 4, "RZ" = 5, "TT" = 6, "other" = 7)

# Trainingsdaten laden
train_data <- read.csv(r"(Data\tables\extracted_values.csv)") %>%
  .[, c("NBR2", "B8A", "MSAVI2", "BSI8A", "MSAVI8A", "RVI_buff", "VH_mean", "class", "date", "BODTYP_K")] %>%
  mutate(BODTYP_K = mapping[BODTYP_K]) %>%
  mutate(class = ifelse(class %in% c(2,3), 2, class)) %>%
  mutate(class = as.factor(class))
# Tiff Dateien laden
# S1
S1_files <- list.files("Data/S1/pro/mosaic/mean/texture", pattern = "\\.tif$", full.names = TRUE)
# S2
S2_files <- list.files("Data/S2/pro/mosaic", pattern = "\\.tif$", full.names = TRUE)

# feldschlaege
FS <- vect(r"(Data\vector\schlaege\schlaege_edit_all_4326.shp)") %>% project("EPSG:32632")
# Bodendaten laden
soil_types <- read.csv("data/tables/soiltypes_aggregated.csv")
soil_data <- terra::vect("Data/vector/vbk/vbk50_2009_salza_fixed.shp") %>%
  project("EPSG:32632") %>%
  tidyterra::mutate(BODTYP_K = ifelse(BODTYP_K %in% soil_types$BODTYP_K, BODTYP_K, "other") %>% mapping[.])

soil_raster <- rasterize(soil_data, rast(S2_files[1]), field = "BODTYP_K", fun = "max") %>% crop(FS, mask=TRUE)
# soil_raster <- values(soil_raster) %>% .[. == NaN] <- NA
# values(soil_raster)[soil_raster == "<NA>"] <- NA
plot(soil_raster)
head(soil_raster)
 unique(values(soil_raster))

# S1 VH und VV moving window buffer für RVI_buff
w <- focalMat(rast(S1_files[1]), 50, type="circle")
sub_start_time <- Sys.time()
for (i in seq_along(S1_files)) {
  cat(sprintf("\rApplying focal mean - Fortschritt: %d von %d (%s)", i, length(S1_files), basename(S1_files[i])))
  name <- basename(S1_files[i])
  raster <- rast(S1_files[i])
  # auf feldschlägen zuschneiden
  raster <- terra::crop(raster, FS, mask = TRUE)
  raster$VH_buff <- focal(raster$VH, w=w, fun=mean, na.rm=TRUE)
  raster$VV_buff <- focal(raster$VV, w=w, fun=mean, na.rm=TRUE)
  raster$RVI_buff <- (4 * raster$VH_buff) / (raster$VH_buff + raster$VV_buff)
  writeRaster(raster[[c("RVI_buff", "VH_mean")]], filename=paste0("Data/S1/pro/mosaic/mean/texture/buff/", name), overwrite=TRUE)
  if (i == length(S1_files)) {
    sub_end_time <- Sys.time()
    cat(sprintf("\rfocal mean calculation finished! Total processing time: %s\n", difftime(sub_end_time, sub_start_time, units = "mins")))
  }
}
#update S1_files
S1_files <- list.files("Data/S1/pro/mosaic/mean/texture/buff", pattern = "\\.tif$", full.names = TRUE)
# S2 inices berechnen
sub_start_time <- Sys.time()
for (i in seq_along(S2_files)) {
  cat(sprintf("\rCalculating S2 indices - Fortschritt: %d von %d (%s)", i, length(S2_files), basename(S2_files[i])))
  name <- basename(S2_files[i])
  raster <- rast(S2_files[i])
  raster <- terra::crop(raster, FS, mask = TRUE)
  raster$NBR2 = (raster$B11 - raster$B12) / (raster$B11 + raster$B12)
  raster$MSAVI2 = (2 * raster$B08 + 1 - sqrt((2 * raster$B08 + 1) ^ 2 - 8 * (raster$B08 - raster$B04))) / 2
  raster$BSI8A = ((raster$B11 + raster$B04) - (raster$B8A + raster$B02)) / ((raster$B11 + raster$B04) + (raster$B8A + raster$B02))
  raster$MSAVI8A = (2 * raster$B8A + 1 - sqrt((2 * raster$B8A + 1) ^ 2 - 8 * (raster$B8A - raster$B04))) / 2

  writeRaster(raster[[c("NBR2", "MSAVI2", "BSI8A", "MSAVI8A", "B8A")]], filename=paste0("Data/S2/pro/mosaic/indices/", name), overwrite=TRUE)
  if (i == length(S2_files)) {
    sub_end_time <- Sys.time()
    cat(sprintf("\rS2 Indices calculation finished! Total processing time: %s\n", difftime(sub_end_time, sub_start_time, units = "mins")))
  }
}
#update S2_files
S2_files <- list.files("Data/S2/pro/mosaic/indices", pattern = "\\.tif$", full.names = TRUE)

# combine S1 and S2
for (i in seq_along(1 : 6)) {
  cat(sprintf("\rCombining S1 and S2 - Fortschritt: %d von %d (%s)", i, 6, basename(S2_files[i])))
  r1 <- rast(S2_files[i])
  date <- substr(basename(S2_files[i]), 12, 17)
  r2 <- rast(S1_files[i]) %>% resample(r1, method = "bilinear")
  combined <- c(r1, r2, soil_raster)
  writeRaster(combined, filename = paste0("Data/mixed/", date, "_S1_S2_stack.tif"), overwrite = TRUE)
  if (i == length(1 : 6)) {
    cat("\rCombining S1 and S2 finished!                                                        \n")
  }
}
# create tif list
mixed_tif <- list.files("Data/mixed", pattern = "\\.tif$", full.names = TRUE)

# Random Forest Modelle trainieren
strata_data <- train_data[, c("class", "BODTYP_K", "date")]

# multi_strata erzeugt die Schichten (Strata)
strata_factor <- multi_strata(strata_data, strategy = "interaction")

# Daten partitionieren mit der stratification
set.seed(161)
partitions <- partition(strata_factor, p = c(train = 0.8, valid = 0.2))
train_data <- train_data[partitions$train, -which(names(train_data) %in% c("date"))]

valid_data <- train_data[partitions$valid, ]
str(train_data)
set.seed(161)
rf_optical <- randomForest(class ~ ., data = train_data[complete.cases(train_data), c("NBR2", "B8A", "MSAVI2", "BSI8A", "class", "BODTYP_K")], ntree = 500)
rf_mixed <- randomForest(class ~ ., data = train_data[complete.cases(train_data[, c("NBR2", "B8A", "MSAVI8A", "RVI_buff", "BODTYP_K", "class")]), c("NBR2", "B8A", "MSAVI8A", "RVI_buff", "BODTYP_K", "class")], ntree = 500)
rf_sar <- randomForest(class ~ ., data = train_data[complete.cases(train_data[, c("RVI_buff", "VH_mean", "BODTYP_K", "class")]), c("RVI_buff", "VH_mean", "BODTYP_K", "class")], ntree = 500)

# Modelle anwenden
for (i in seq_along(mixed_tif)) {
  cat(sprintf("\rClassifying - Fortschritt: %d von %d (%s)", i, length(mixed_tif), basename(mixed_tif[i])))
  raster <- rast(mixed_tif[i])
  classified <- predict(raster[[c("NBR2", "B8A", "MSAVI2", "BSI8A", "BODTYP_K")]], rf_optical, type = "response", na.rm = TRUE)
  values(classified) <- as.numeric(values(classified))
  writeRaster(classified, filename = paste0("Data/classified/", str_sub(basename(mixed_tif[i]), end = -16), "optical_classified.tif"), overwrite = TRUE)
  classified <- predict(raster[[c("RVI_buff", "VH_mean", "BODTYP_K")]], rf_sar, type = "response", na.rm = TRUE)
  values(classified) <- as.numeric(values(classified))
  writeRaster(classified, filename = paste0("Data/classified/", str_sub(basename(mixed_tif[i]), end = -16), "sar_classified.tif"), overwrite = TRUE)
  classified <- predict(raster[[c("NBR2", "B8A", "MSAVI8A", "RVI_buff", "BODTYP_K")]], rf_mixed, type = "response", na.rm = TRUE)
  values(classified) <- as.numeric(values(classified))
  writeRaster(classified, filename = paste0("Data/classified/", str_sub(basename(mixed_tif[i]), end = -16), "mixed_classified.tif"), overwrite = TRUE)
  if (i == length(mixed_tif)) {
    cat("\rClassification finished!                                                        \n")
  }
}
