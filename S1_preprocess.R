# S1 Preprocessing Script
# This script preprocesses Sentinel-1 GRD data using SNAP's Graph Processing Tool (GPT).
# It includes steps for subsetting, border noise removal, calibration, terrain correction, and conversion to dB scale.
time_start <- Sys.time()
packages <- c("tidyr", "dplyr", "stringr", "xml2", "terra", "glcm", "pbapply", "parallel")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}

# available files:
zip_files <- list.files("Data/S1/RAW", pattern = "\\.zip$", full.names = TRUE)
zip_files <- zip_files[-c(7,14)] # remove files that do not cover the whole area
# workflow file path
wf_single <- r"(C:\Demian\Uni_Halle\S2-S25\LSS1\working_directory\Data\xmlGraphs\Filipponi_S1_GRD_preprocessing_modified-DG_single.xml)"

# WKT polygon laden
WKT <- readLines("data/tables/ref_data_extent_epsg4326_buff30m.wkt")

# loop through each file and apply the workflow
for (file in zip_files) {

  # Define input and output file paths
  input_file <- file
  output_file <- sub("RAW", "pro", sub("\\.zip$", "", file))
  print(paste("Processing file:    ", input_file))
  print(paste("Output file will be:", output_file))

  # Construct the GPT command
  cmd <- paste("gpt", wf_single,
               paste0("-Pinput=\"", input_file, "\""),
               "-Pfilter=\"None\"",
               "-Pdem=\"SRTM 3Sec\"",
               "-Presolution=10",
               "-Porigin=5",
               paste0("-PpolygonRegion=\"", WKT, "\""),
               paste0("-Poutput=\"", output_file, "\""),
               "-Pformat=\"GeoTIFF-Bigtiff\"")

  # Execute the GPT command
  system(cmd)
}

# load tif_files 
tif_files <- list.files("Data/S1/pro", pattern = "\\.tif$", full.names = TRUE)

for (i in seq_along(tif_files)) {
  cat(sprintf("\rReproject to project-crs and renaming layers in file %d of %d", i, length(tif_files)))
  img <- rast(tif_files[i])
  names(img) <- c("VH", "VV")

  # Project to Project crs
  img <- terra::project(img, "EPSG:32632")
  
  # Erstelle eine temporäre Datei
  temp_file <- tempfile(fileext = ".tif")
  
  # Schreibe das veränderte Raster dorthin
  writeRaster(img, filename = temp_file, overwrite = TRUE)
  
  # Überschreibe die Originaldatei mit der temporären Datei
  file.copy(temp_file, tif_files[i], overwrite = TRUE)
  
  # Optional: lösche die temporäre Datei
  file.remove(temp_file)
  if (i == length(tif_files)) {
    cat("\rRenaming and reprojection finished!                                                                        \n")
  }
}

# create dataframe with file names and dates
df <- data.frame(file = basename(tif_files), date = substr(basename(tif_files), 18, 25), stringsAsFactors = FALSE)

# Mosaicing subswaths with the same date
# find duplicates and mosaic them
unique_dates <- unique(df$date)

for (i in seq_along(unique_dates)) {
  current_date <- unique_dates[i]
  if (sum(df$date == current_date) > 1) {
    print(paste("Mosaicing date:", current_date))
    index <- which(df$date == current_date)
    # mosaicing
    mosaic <- mosaic(rast(tif_files[index[1]]), rast(tif_files[index[2]]), fun = "mean")
    writeRaster(mosaic,
                filename = paste0("Data/S1/pro/mosaic/",
                                  str_sub(basename(tif_files[index[1]]), end = -9),
                                  "mosaic.tif"),
                overwrite = TRUE)
  } else {
    print(paste("No mosaicing needed for date:", current_date, "creating file.link if not existing"))
    index <- which(df$date == current_date)
    # move non-mosaic files
    if (!file.exists(paste0("Data/S1/pro/mosaic/", basename(tif_files[index])))) {
      file.link(tif_files[index], paste0("Data/S1/pro/mosaic/", basename(tif_files[index])))
    }
  }
}
# actuallise tif_files
tif_files <- list.files("Data/S1/pro/mosaic/", pattern = "\\.tif$", full.names = TRUE)
S1_dates <- data.frame(date = as.Date(substr(basename(tif_files), 18, 25), format = "%Y%m%d"))

rast(tif_files[9])

# tifs mitteln
S2_dates <- unique(read.csv("data/tables/S2_dates.csv"))

# find closest S1 scenes and calculate mean
for (i in seq_along(S2_dates$date)) {
  current_date <- as.Date(S2_dates$date[i], format = "%Y-%m-%d")
  print(paste("Processing reference date:", current_date))

  diffs <- abs(as.numeric(S1_dates$date - current_date))

  indices <- which(diffs < 4)[order(diffs[diffs < 4])[1:2]]

  print(paste("Closest S1 dates to", current_date, "are:",
              as.Date(S1_dates$date[indices[1]]), "with difference", diffs[indices[1]],
              "and",
              as.Date(S1_dates$date[indices[2]]), "with difference", diffs[indices[2]]))
  # Mean calculation

  if (sum(!is.na(indices)) == 2) {
    print("Calculating mean raster")
    rast1 <- rast(tif_files[indices[1]])
    rast2 <- rast(tif_files[indices[2]])
    if (nlyr(rast1) != nlyr(rast2)) stop("Layer number differs! No recycling permitted.")
    mean_rast <- (rast1 + rast2) / 2
    writeRaster(mean_rast,
                filename = paste0("Data/S1/pro/mosaic/mean/S1A_IW_GRDH_1SDV_",
                                  substr(basename(tif_files[indices[1]]), 18, 25),
                                  "-",
                                  substr(basename(tif_files[indices[2]]), 18, 25),
                                  "_mean.tif"),
                overwrite = TRUE)
  } else if (sum(!is.na(indices)) == 1) {
    print("Linking single raster")
    if (!file.exists(paste0("Data/S1/pro/mosaic/mean/", basename(tif_files[indices[1]])))) {

      file.link(tif_files[indices[1]], paste0("Data/S1/pro/mosaic/mean/", basename(tif_files[indices[1]])))
    }
  } else {
    print("No valid indices found")
  }
}
end_time <- Sys.time()
cat(sprintf("Total processing time: %s\n", difftime(end_time, start_time, units = "mins")))



str(S1_dates)
str(current_date)
window_size <- 3
# Struktur / texturmerkmale berechnen
tif_files <- list.files("Data/S1/pro/mosaic/mean/", pattern = "\\.tif$", full.names = TRUE)
#tif_files <- tif_files[1]
sub_start_time <- Sys.time()
for (tif_file in seq_along(tif_files)) {
  cat(sprintf("\rCalculating texture-parameters for file %d of %d", tif_file, length(tif_files)))
  rast <- rast(tif_files[tif_file])
  # maske zum maskieren von Randbereichen erzeugen
  mask <- rast[[1]] # beliebiges band
  values(mask) <- ifelse(is.na(values(rast[[1]])), 1, NA)
  mask_buff <- terra::buffer(mask, width = window_size * 5) # window_size * 5 da fenstergröße in pixel und buffer in metern
  n_layer <- dim(rast)[3]
  # calculate glcm for each layer
  for (layer in 1:n_layer) { # jeder Layer einzeln (VH,VV)
    layer <- rast[[layer]]
    band_name <- names(layer)
    mat <- as.matrix(layer, wide = TRUE)
    mat_clean <- mat
    mat_clean[is.na(mat_clean)] <- -999
    glcm_raster <- glcm::glcm(mat_clean, window = c(window_size, window_size), na_opt = "any", na_val = NA)
    glcm_names <- dimnames(glcm_raster)[[3]]
    n_glcm <- dim(glcm_raster)[3]
    # umwandeln in spatraster, maskieren und dem original raster hinzufügen
    for (layer_glcm in 1:n_glcm) { # jeden GLCM Layer einzeln in spatraster umwandeln und dem spatraster hinzufügen
      mat_layer <- glcm_raster[, , layer_glcm]
      rast_new <- rast(
        nrows = nrow(rast),
        ncols = ncol(rast),
        xmin = xmin(rast),
        xmax = xmax(rast),
        ymin = ymin(rast),
        ymax = ymax(rast),
        crs = crs(rast)
      )
      # Werte setzen (als Vektor, beachtet Reihenfolge)
      values(rast_new) <- as.vector(t(mat_layer))
      # Randbereiche maskieren
      masked <- terra::mask(rast_new, mask_buff, maskvalues = 1, updatevalue = NA)
      #values(rast_new) <- ifelse(!is.na(values(mask_buff)), NA, values(rast_new))
      # Namen setzen
      names(masked) <- paste0(band_name, substr(glcm_names[layer_glcm], 5, nchar(glcm_names[layer_glcm])))
      rast <- c(rast, masked)
    }
  }
  writeRaster(rast,
              filename = paste0("Data/S1/pro/mosaic/mean/texture/", substr(basename(tif_files[tif_file]), 1, 25), ".tif"),
              overwrite = TRUE)
  if (tif_file == length(tif_files)) {
    sub_end_time <- Sys.time()
    cat(sprintf("\rTexture calculation finished! Total processing time for subset: %s\n", difftime(sub_end_time, sub_start_time, units = "mins")))
  }
}

names(rast_new)
str(glcm_raster)
library(terra)
library(glcm)
library(parallel)

tif_files <- list.files("Data/S1/pro/mosaic/mean/", pattern = "\\.tif$", full.names = TRUE)

process_file <- function(tif_file, window_size) {
  rast <- rast(tif_file)

  mask <- rast[[1]] # beliebiges band
  values(mask) <- ifelse(is.na(values(rast[[1]])), 1, NA)
  mask_buff <- terra::buffer(mask, width = window_size * 5) # window_size * 5 da fenstergröße in pixel und buffer in metern

  n_layer <- dim(rast)[3]

  for (layer in 1:n_layer) {
    band <- rast[[layer]]
    band_name <- names(band)
    mat <- as.matrix(band, wide = TRUE)
    mat[is.na(mat)] <- -999

    glcm_raster <- glcm::glcm(mat, window = c(window_size, window_size), na_opt = "any", na_val = NA)

    glcm_names <- dimnames(glcm_raster)[[3]]
    n_glcm <- dim(glcm_raster)[3]
    
    for (layer_glcm in 1:n_glcm) {
      mat_layer <- glcm_raster[, , layer_glcm]
      rast_new <- rast(nrows = nrow(rast), ncols = ncol(rast),
                       xmin = xmin(rast), xmax = xmax(rast),
                       ymin = ymin(rast), ymax = ymax(rast),
                       crs = crs(rast))
      values(rast_new) <- as.vector(t(mat_layer))

      masked <- terra::mask(rast_new, mask_buff, maskvalues = 1, updatevalue = NA)

      names(masked) <- paste0(band_name, substr(glcm_names[layer_glcm], 5, nchar(glcm_names[layer_glcm])))

      rast <- c(rast, masked)
    }
  }
  out <- file.path("Data/S1/pro/mosaic/mean/texture",
                   paste0(substr(basename(tif_file), 1, 25), ".tif"))
  writeRaster(rast, out, overwrite = TRUE)
}

# nutzt alle Kerne außer 1
n_cores <- max(1, detectCores() - 1)
pblapply(tif_files, FUN = process_file, cl = n_cores, window_size = 3)
