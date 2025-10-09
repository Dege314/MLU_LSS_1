packages <- c("randomForest", "terra", "dplyr", "ggplot2", "ggpubr")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}
# Klassifizierung der S1 und S2 Szenen mit RF

# Random Forest Modelle laden
# optical
# SAR
# mixed

# Tiff Dateien laden
# S1
S1_files <- list.files("Data/S1/pro/mosaic/mean/texture", pattern = "\\.tif$", full.names = TRUE)
names(rast(S1_files[1]))
# S2
S2_files <- list.files("Data/S2/pro/mosaic", pattern = "\\.tif$", full.names = TRUE)

# S1 VH und VV moving window buffer für RVI_buff
w <- focalMat(rast(S1_files[1]), 50, type="circle")
sub_start_time <- Sys.time()
for (i in 1:length(S1_files)) {
  cat(sprintf("\rApplying focal mean - Fortschritt: %d von %d (%s)", i, length(S1_files), basename(S1_files[i])))
  name <- basename(S1_files[i])
  raster <- rast(S1_files[i])
  raster$VH_buff <- focal(raster$VH, w=w, fun=mean, na.rm=TRUE)
  raster$VV_buff <- focal(raster$VV, w=w, fun=mean, na.rm=TRUE)
  raster$RVI_buff <- (4 * raster$VH_buff) / (raster$VH_buff + raster$VV_buff)
  writeRaster(raster[[c("RVI_buff", "VH_mean")]], filename=paste0("Data/S1/pro/mosaic/mean/texture/buff/", name), format="GTiff", overwrite=TRUE)
  if (i == length(S1_files)) {
    sub_end_time <- Sys.time()
    cat(sprintf("\rfocal mean calculation finished! Total processing time: %s\n", difftime(sub_end_time, sub_start_time, units = "mins")))
  }
}
# Datenvormat eventuell anpassen für RF

# Random Forest Modelle anwenden pro zeitpunkt