# S1 Preprocessing Script
# This script preprocesses Sentinel-1 GRD data using SNAP's Graph Processing Tool (GPT).
# It includes steps for subsetting, border noise removal, calibration, terrain correction, and conversion to dB scale.
packages <- c("tidyr", "dplyr", "stringr", "xml2", "terra")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}

# available files:
zip_files <- list.files("Data/S2/RAW", pattern = "\\.zip$", full.names = TRUE)
zip_files <- zip_files[1]
# workflow file path
wf <- r"(C:\Demian\Uni_Halle\S2-S25\LSS1\working_directory\Data\xmlGraphs\S2_subset_resample_tif.xml)"
# WKT polygon laden
WKT <- readLines("data/tables/ref_data_extent_epsg4326_buff30m.wkt")
bands <- "B1,B2,B3,B4,B5,B6,B7,B8,B8A,B11,B12,SCL"

# loop through each file and apply the workflow
for (file in zip_files) {

  # Define input and output file paths
  input_file <- file
  output_file <- sub("RAW", "pro", sub("\\.zip$", "", file))
  print(paste("Processing file:    ", input_file))
  print(paste("Output file will be:", output_file))

  # Construct the GPT command
  cmd <- paste("gpt", wf,
               paste0("-Pinput=\"", input_file, "\""),
#               paste0("-PbandNames=\"", bands, "\""),
               paste0("-PpolygonRegion=\"", WKT, "\""),
               paste0("-Poutput=\"", output_file, "\""),
               "-Pformat=\"GeoTIFF-Bigtiff\"")

  # Execute the GPT command
  system(cmd)
}
# B1,B2,B3,B4,B5,B6,B7,B8,B8A,B11,B12,
# Mosaicing subswaths with the same date
# load tif_files 
tif_files <- list.files("Data/S2/pro", pattern = "\\.tif$", full.names = TRUE)

# create dataframe with file names and dates
df <- data.frame(file = basename(tif_files), date = substr(basename(tif_files), 12, 19), stringsAsFactors = FALSE)

# find duplicates and mosaic them
unique_dates <- unique(df$date)

for (i in 1:length(unique_dates)) {
  current_date <- unique_dates[i]
  if (sum(df$date == current_date) > 1) {
    index <- which(df$date == current_date)
    print(index)
    # mosaicing
    mosaic(rast(tif_files[index[1]]), rast(tif_files[index[2]]), fun = "mean") %>%
      writeRaster(filename = paste0("Data/S2/pro/", str_sub(basename(tif_files[index[1]]), end = -42), "mosaic.tif"),
                  overwrite = TRUE)
    # move mosaic_basefile
    file.rename(tif_files[index], paste0("Data/S2/pro/mosaic_basefile/", basename(tif_files[index])))
  }
}
# actuallise tif_files
tif_files <- list.files("Data/S2/pro", pattern = "\\.tif$", full.names = TRUE)

