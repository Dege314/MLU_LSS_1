# S1 Preprocessing Script
# This script preprocesses Sentinel-1 GRD data using SNAP's Graph Processing Tool (GPT).
# It includes steps for subsetting, border noise removal, calibration, terrain correction, and conversion to dB scale.

# available files:
tif_files <- list.files("Data/S1/RAW", pattern = "\\.zip$", full.names = TRUE)
tif_files <- tif_files[3:4]
# workflow file path
workflow <- r"(C:\Demian\Uni_Halle\S2-S25\LSS1\working_directory\Data\xmlGraphs\Filipponi_S1_GRD_preprocessing_modified-DG.xml)"

# loop through each file and apply the workflow
for (file in tif_files) {
  # Define input and output file paths
  input_file <- file
  output_file <- sub("RAW", "pro", sub("\\.zip$", "", file))
  print(paste("Processing file:    ", input_file))
  print(paste("Output file will be:", output_file))
  # Construct the GPT command
  cmd <- paste("gpt", workflow,
               paste0("-Pinput=\"", input_file, "\""),
               "-Pfilter=\"None\"",
               "-Pdem=\"SRTM 3Sec\"",
               "-Presolution=10",
               "-Porigin=5",
               paste0("-PpolygonRegion=\"POLYGON((",
                      "11.3662944434413 51.3149803572737, ",
                      "11.3662944434413 51.5960938906235, ",
                      "11.8849284168966 51.5960938906235, ",
                      "11.8849284168966 51.3149803572737, ",
                      "11.3662944434413 51.3149803572737))\""),
               paste0("-Poutput=\"", output_file, "\""),
               "-Pformat=\"GeoTIFF-BigTIFF\"")
  
  # Execute the GPT command
  system(cmd)
}

# stop execution after loop
stop("Processing complete.")

# For singel use:
input_file <- r"(Data\S1\RAW\S1A_IW_GRDH_1SDV_20180423T052518_20180423T052543_021590_025363_7309.zip)"
output_file <- r"(Data\S1\pro\S1A_IW_GRDH_1SDV_20180423T052518_20180423T052543_021590_025363_7309)"
cmd <- paste("gpt", workflow,
             paste0("-Pinput=\"", input_file, "\""),
             "-Pfilter=\"None\"",
             "-Pdem=\"SRTM 3Sec\"",
             "-Presolution=10",
             "-Porigin=5",
             paste0("-PpolygonRegion=\"POLYGON((",
                    "11.3662944434413 51.3149803572737, ",
                    "11.3662944434413 51.5960938906235, ",
                    "11.8849284168966 51.5960938906235, ",
                    "11.8849284168966 51.3149803572737, ",
                    "11.3662944434413 51.3149803572737))\""),
             paste0("-Poutput=\"", output_file, "\""),
             "-Pformat=\"GeoTIFF-BigTIFF\"")
system(cmd)