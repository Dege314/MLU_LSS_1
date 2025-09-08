# S1 Preprocessing Script
# This script preprocesses Sentinel-1 GRD data using SNAP's Graph Processing Tool (GPT).
# It includes steps for subsetting, border noise removal, calibration, terrain correction, and conversion to dB scale.
packages <- c("tidyr", "dplyr", "stringr", "xml2")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}

# available files:
tif_files <- list.files("Data/S1/RAW", pattern = "\\.zip$", full.names = TRUE)
tif_files <- tif_files[3:4]
# workflow file path
wf_single <- r"(C:\Demian\Uni_Halle\S2-S25\LSS1\working_directory\Data\xmlGraphs\Filipponi_S1_GRD_preprocessing_modified-DG_single.xml)"
wf_mosaic <- r"(C:\Demian\Uni_Halle\S2-S25\LSS1\working_directory\Data\xmlGraphs\Filipponi_S1_GRD_preprocessing_modified-DG_mosaic.xml)"

# devide tif files into single and multiszene per date
dates <- substr(basename(tif_files), 18, 25)

# Dataframe mit Datei und Datum
df <- data.frame(file = tif_files, date = dates, stringsAsFactors = FALSE)

# Dateien nach Datum gruppieren und Anzahl zählen
grouped <- df %>%
  group_by(date) %>%
  summarise(files = list(file), count = n())

# Liste mit einzelnen Dateien pro Datum (count == 1)
single_files <- grouped %>%
  filter(count == 1) %>%
  pull(files) %>%
  unlist()

# Liste mit mehreren Dateien pro Datum (count > 1)
multi_files <- grouped %>%
  filter(count > 1) %>%
  pull(files)

# Ausgabe
cat("Dateien mit nur einem Eintrag pro Datum:\n")
print(single_files)

cat("\nGruppierte Dateien pro Datum mit mehreren Einträgen:\n")
print(multi_files)

# loop through each file and apply the workflow
for (file in tif_files) {
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
               paste0("-PpolygonRegion=\"POLYGON((",
                      "11.3662944434413 51.3149803572737, ",
                      "11.3662944434413 51.5960938906235, ",
                      "11.8849284168966 51.5960938906235, ",
                      "11.8849284168966 51.3149803572737, ",
                      "11.3662944434413 51.3149803572737))\""),
               paste0("-Poutput=\"", output_file, "\""),
               "-Pformat=\"BEAM-DIMAP\"")
  
  # Execute the GPT command
  system(cmd)
}

# loop through each file group and apply the mosaic workflow
for (i in seq_along(multi_files)) {
  cat("Prozessiere Datumgruppe", i, "mit Dateien:\n")
  print(multi_files[[i]])

  # Define input and output file paths
  input_file_1 <- multi_files[[i]][1]
  input_file_2 <- multi_files[[i]][2]
  output_file <- paste0("Data/S1/pro/", str_sub(basename(input_file_1), end = -9), "mosaic")
  print(paste("Output file will be:", output_file))
  # Construct the GPT command
  cmd <- paste("gpt", wf_single,
               paste0("-Pinput1=\"", input_file_2, "\""),
               paste0("-Pinput2=\"", input_file_1, "\""),
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
            #    paste0("-Poutput1=\"", output_file, "1", "\""),
            #    paste0("-Poutput2=\"", output_file, "2", "\""),
               paste0("-Poutput=\"", output_file, "\""),
               "-Pformat=\"BEAM-DIMAP\"")
  
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

############ test #################
workflow <-r"(Data\xmlGraphs\test.xml)"
input1 <- r"(Data/S1/pro/S1A_IW_GRDH_1SDV_20200918T170007_20200918T170032_034416_0400BC_5B02.dim)"
input2 <- r"(Data/S1/pro/S1A_IW_GRDH_1SDV_20200918T170032_20200918T170057_034416_0400BC_9078.dim)"
cmd <- paste("gpt", workflow)
,
             paste0("-Pinput1=\"", input1, "\""),
             paste0("-Pinput2=\"", input2, "\""),
             "-Poutput=\"Data/S1/pro/mosaic_test\"",
             "-Pformat=\"GeoTIFF-BigTIFF\"",
                          paste0("-PpolygonRegion=\"POLYGON((",
                    "11.3662944434413 51.3149803572737, ",
                    "11.3662944434413 51.5960938906235, ",
                    "11.8849284168966 51.5960938906235, ",
                    "11.8849284168966 51.3149803572737, ",
                    "11.3662944434413 51.3149803572737))\""))
             system(cmd)


############# automatic xml creation #############

library(xml2)

generate_snap_graph <- function(input_files) {
  graph <- xml_new_root("graph", id = "S1_GRD_preprocessing")
  xml_add_child(graph, "version", "1.0")
  
  create_processing_nodes <- function(graph, id_suffix, input_var) {
    read_id <- paste0("Read", id_suffix)
    read_node <- xml_add_child(graph, "node", id = read_id)
    xml_add_child(read_node, "operator", "Read")
    xml_add_child(read_node, "sources")
    params <- xml_add_child(read_node, "parameters", class = "com.bc.ceres.binding.dom.XppDomElement")
    xml_add_child(params, "file", input_var)
    
    orbit_id <- paste0("Apply-Orbit-File", id_suffix)
    orbit_node <- xml_add_child(graph, "node", id = orbit_id)
    xml_add_child(orbit_node, "operator", "Apply-Orbit-File")
    sources <- xml_add_child(orbit_node, "sources")
    xml_add_child(sources, "sourceProduct", refid = read_id)
    orbit_params <- xml_add_child(orbit_node, "parameters", class = "com.bc.ceres.binding.dom.XppDomElement")
    xml_add_child(orbit_params, "orbitType", "Sentinel Precise (Auto Download)")
    xml_add_child(orbit_params, "polyDegree", "3")
    xml_add_child(orbit_params, "continueOnFail", "true")
    
    noise_id <- paste0("ThermalNoiseRemoval", id_suffix)
    noise_node <- xml_add_child(graph, "node", id = noise_id)
    xml_add_child(noise_node, "operator", "ThermalNoiseRemoval")
    sources <- xml_add_child(noise_node, "sources")
    xml_add_child(sources, "sourceProduct", refid = orbit_id)
    noise_params <- xml_add_child(noise_node, "parameters", class = "com.bc.ceres.binding.dom.XppDomElement")
    xml_add_child(noise_params, "selectedPolarisations")
    xml_add_child(noise_params, "removeThermalNoise", "true")
    xml_add_child(noise_params, "reIntroduceThermalNoise", "false")
    
    border_id <- paste0("Remove-GRD-Border-Noise", id_suffix)
    border_node <- xml_add_child(graph, "node", id = border_id)
    xml_add_child(border_node, "operator", "Remove-GRD-Border-Noise")
    sources <- xml_add_child(border_node, "sources")
    xml_add_child(sources, "sourceProduct", refid = noise_id)
    border_params <- xml_add_child(border_node, "parameters", class = "com.bc.ceres.binding.dom.XppDomElement")
    xml_add_child(border_params, "selectedPolarisations")
    xml_add_child(border_params, "borderLimit", "500")
    xml_add_child(border_params, "trimThreshold", "50")
    
    calib_id <- paste0("Calibration-Sigma", id_suffix)
    calib_node <- xml_add_child(graph, "node", id = calib_id)
    xml_add_child(calib_node, "operator", "Calibration")
    sources <- xml_add_child(calib_node, "sources")
    xml_add_child(sources, "sourceProduct", refid = border_id)
    calib_params <- xml_add_child(calib_node, "parameters", class = "com.bc.ceres.binding.dom.XppDomElement")
    xml_add_child(calib_params, "sourceBands", "Intensity_VH, Intensity_VV")
    xml_add_child(calib_params, "auxFile", "Product Auxiliary File")
    xml_add_child(calib_params, "externalAuxFile")
    xml_add_child(calib_params, "outputImageInComplex", "false")
    xml_add_child(calib_params, "outputImageScaleInDb", "false")
    xml_add_child(calib_params, "createGammaBand", "false")
    xml_add_child(calib_params, "createBetaBand", "false")
    xml_add_child(calib_params, "selectedPolarisations")
    xml_add_child(calib_params, "outputSigmaBand", "true")
    xml_add_child(calib_params, "outputGammaBand", "false")
    xml_add_child(calib_params, "outputBetaBand", "false")
    
    speckle_id <- paste0("Speckle-Filter", id_suffix)
    speckle_node <- xml_add_child(graph, "node", id = speckle_id)
    xml_add_child(speckle_node, "operator", "Speckle-Filter")
    sources <- xml_add_child(speckle_node, "sources")
    xml_add_child(sources, "sourceProduct", refid = calib_id)
    speckle_params <- xml_add_child(speckle_node, "parameters", class = "com.bc.ceres.binding.dom.XppDomElement")
    xml_add_child(speckle_params, "sourceBands", "Sigma0_VH, Sigma0_VV")
    xml_add_child(speckle_params, "filter", "${filter}")
    xml_add_child(speckle_params, "filterSizeX", "3")
    xml_add_child(speckle_params, "filterSizeY", "3")
    xml_add_child(speckle_params, "dampingFactor", "2")
    xml_add_child(speckle_params, "estimateENL", "true")
    xml_add_child(speckle_params, "enl", "1.0")
    xml_add_child(speckle_params, "numLooksStr", "1")
    xml_add_child(speckle_params, "windowSize", "7x7")
    xml_add_child(speckle_params, "targetWindowSizeStr", "3x3")
    xml_add_child(speckle_params, "sigmaStr", "0.9")
    xml_add_child(speckle_params, "anSize", "50")
    
    terrain_id <- paste0("Terrain-Correction", id_suffix)
    terrain_node <- xml_add_child(graph, "node", id = terrain_id)
    xml_add_child(terrain_node, "operator", "Terrain-Correction")
    sources <- xml_add_child(terrain_node, "sources")
    xml_add_child(sources, "sourceProduct", refid = speckle_id)
    terrain_params <- xml_add_child(terrain_node, "parameters")
    xml_add_child(terrain_params, "sourceBands", "Sigma0_VH, Sigma0_VV")
    xml_add_child(terrain_params, "demName", "${dem}")
    xml_add_child(terrain_params, "externalDEMNoDataValue", "0.0")
    xml_add_child(terrain_params, "externalDEMApplyEGM", "true")
    xml_add_child(terrain_params, "demResamplingMethod", "BILINEAR_INTERPOLATION")
    xml_add_child(terrain_params, "imgResamplingMethod", "BILINEAR_INTERPOLATION")
    xml_add_child(terrain_params, "pixelSpacingInMeter", "${resolution}")
    xml_add_child(terrain_params, "mapProjection")
    xml_add_child(terrain_params, "alignToStandardGrid", "true")
    xml_add_child(terrain_params, "standardGridOriginX", "${origin}")
    xml_add_child(terrain_params, "standardGridOriginY", "${origin}")
    xml_add_child(terrain_params, "nodataValueAtSea", "false")
    xml_add_child(terrain_params, "saveDEM", "false")
    xml_add_child(terrain_params, "saveLatLon", "false")
    xml_add_child(terrain_params, "saveIncidenceAngleFromEllipsoid", "false")
    xml_add_child(terrain_params, "saveLocalIncidenceAngle", "false")
    xml_add_child(terrain_params, "saveProjectedLocalIncidenceAngle", "false")
    xml_add_child(terrain_params, "saveSelectedSourceBand", "true")
    xml_add_child(terrain_params, "outputComplex", "false")
    xml_add_child(terrain_params, "applyRadiometricNormalization", "false")
    xml_add_child(terrain_params, "saveSigmaNought", "false")
    xml_add_child(terrain_params, "saveGammaNought", "false")
    xml_add_child(terrain_params, "saveBetaNought", "false")
    xml_add_child(terrain_params, "incidenceAngleForSigma0", "Use projected local incidence angle from DEM")
    xml_add_child(terrain_params, "incidenceAngleForGamma0", "Use projected local incidence angle from DEM")
    xml_add_child(terrain_params, "auxFile", "Latest Auxiliary File")
    
    linear_id <- paste0("LinearToFromdB", id_suffix)
    linear_node <- xml_add_child(graph, "node", id = linear_id)
    xml_add_child(linear_node, "operator", "LinearToFromdB")
    sources <- xml_add_child(linear_node, "sources")
    xml_add_child(sources, "sourceProduct", refid = terrain_id)
    linear_params <- xml_add_child(linear_node, "parameters", class = "com.bc.ceres.binding.dom.XppDomElement")
    xml_add_child(linear_params, "sourceBands", "Sigma0_VH, Sigma0_VV")
    
    return(linear_id)
  }
  
  linear_refs <- character(length(input_files))
  for (i in seq_along(input_files)) {
    linear_refs[i] <- create_processing_nodes(graph, as.character(i), paste0("${input", i, "}"))
  }
  
  # Nur Mosaic anlegen, wenn mehr als 1 Input
  if (length(input_files) > 1) {
    mosaic_node <- xml_add_child(graph, "node", id = "Mosaic")
    xml_add_child(mosaic_node, "operator", "Mosaic")
    sources <- xml_add_child(mosaic_node, "sources")
    for (ref in linear_refs) {
      xml_add_child(sources, "sourceProduct", refid = ref)
    }
    mosaic_params <- xml_add_child(mosaic_node, "parameters")
    xml_add_child(mosaic_params, "combine", "First")
    xml_add_child(mosaic_params, "resampling", "Nearest")
    last_ref <- "Mosaic"
  } else {
    last_ref <- linear_refs[1]
  }
  
  # Subset Node
  subset_node <- xml_add_child(graph, "node", id = "Subset")
  xml_add_child(subset_node, "operator", "Subset")
  sources <- xml_add_child(subset_node, "sources")
  xml_add_child(sources, "sourceProduct", refid = last_ref)
  subset_params <- xml_add_child(subset_node, "parameters")
  xml_add_child(subset_params, "geoRegion", "${polygonRegion}")
  xml_add_child(subset_params, "copyMetadata", "true")
  
  # Write Node
  write_node <- xml_add_child(graph, "node", id = "Write")
  xml_add_child(write_node, "operator", "Write")
  sources <- xml_add_child(write_node, "sources")
  xml_add_child(sources, "sourceProduct", refid = "Subset")
  write_params <- xml_add_child(write_node, "parameters", class = "com.bc.ceres.binding.dom.XppDomElement")
  xml_add_child(write_params, "file", "${output}")
  xml_add_child(write_params, "formatName", "${format}")
  
  # XML speichern
  write_xml(graph, "S1_GRD_preprocessing_dynamic.xml")
  cat("XML 'S1_GRD_preprocessing_dynamic.xml' erzeugt.\n")
  
  invisible(graph)
}

# Beispielaufruf:
input_files_example <- c("input1.dim")      # Ein Input -> kein Mosaic
generate_snap_graph(input_files_example)

input_files_example2 <- c("input1.dim", "input2.dim")  # Zwei Inputs -> Mosaic
generate_snap_graph(input_files_example2)
