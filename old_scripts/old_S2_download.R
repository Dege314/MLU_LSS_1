# download sentinel2 data
packages <- c("CDSE", "usethis", "sf", "openeo")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}
Sys.getenv("CDSE_CLIENT_ID") # in der .renviron hinterlegt mit paket usethis
Sys.getenv("CDSE_CLIENT_SECRET") # in der .renviron hinterlegt mit paket usethis

# OAuth-Client erzeugen
client <- GetOAuthClient(id = Sys.getenv("CDSE_CLIENT_ID"), secret = Sys.getenv("CDSE_CLIENT_SECRET"))
class(client)
collections <- GetCollections(as_data_frame = TRUE)

aoi <- st_as_sfc(readLines("Data/tables/ref_data_extent_epsg4326.wkt"), crs = 4326)
token <- GetOAuthToken(id = Sys.getenv("CDSE_CLIENT_ID"), secret = Sys.getenv("CDSE_CLIENT_SECRET"))
time_range <- c("2018-04-22", "2018-04-22")

# CDSE Paket scheinbar nicht geeigent - unterstützt nur Bereiche die kleiner sind als das Untersuchungsgebiet

# script_file <- system.file("scripts", "RawBands.js", package = "CDSE")

# SearchCatalog(aoi = aoi, from = "2018-04-22", to = "2018-04-22",
#               collection = "sentinel-2-l2a", with_geometry = TRUE, client = client)

# GetImage(
#   aoi = aoi,
#   time_range = time_range,
#   collection = "sentinel-2-l2a",
#   script = script_file,
#   resolution = 10,
#   client = client,
#   token = token

# )
# CDSE Paket scheinbar nicht geeigent - unterstützt nur Bereiche die kleiner sind als das Untersuchungsgebiet


SearchCatalog(
  client = client,
  bbox = c(11.5, 51.3, 11.7, 51.5), # Beispielkoordinaten, anpassen!
  time_range = c("2025-04-01", "2025-06-08"),
  product_type = "GRD",
  orbit_direction = "ASCENDING"
)
s <- stac("https://catalogue.dataspace.copernicus.eu/v1/")
s <- stac("https://stac.dataspace.copernicus.eu/v1/")
items <- s %>%
  stac_search(collections = "sentinel-2-l2a",
              bbox = c(13.0, 52.3, 13.8, 52.7),
              datetime = "2023-06-01/2023-06-30",
              limit = 5) %>%
  get_request()

items$features[[1]]$assets$data$href


# Beispiel-AOI als sf-Objekt erzeugen

# Bounding Box: xmin, ymin, xmax, ymax
bbox <- st_bbox(c(xmin = 14.25, ymin = 52.37, xmax = 14.32, ymax = 52.42), crs = st_crs(4326))
aoi <- st_as_sfc(bbox)

# Alternativ als DataFrame für viele rsi Funktionen:
aoi_df <- st_sf(geometry = aoi)

# Sentinel-2-Bilder für September & Oktober 2023 als TIFF herunterladen
sa_sentinel2 <- get_sentinel2_imagery(
  aoi,
  start_date = "2023-09-01",
  end_date = "2023-10-31",
  output_filename = "output_s2.tif"
)


# sits
sentinel2_ts <- sits_get_data(
  service = "cdse_sentinel2_l2a",
  bands = c("B02", "B03", "B04", "B08"), # blaue, grüne, rote, NIR Bänder
  sfc = aoi,
  start_date = "2025-06-01",
  end_date = "2025-08-31",
  output_dir = "S2"
)
sits_list_collections()

library(sf)

# Beispiel Region of Interest (ROI) als bbox
roi <- st_bbox(c(xmin = 14.25, ymin = 52.37, xmax = 14.32, ymax = 52.42), crs = 4326)

cube <- sits_cube(
  source = "cdse",
  collection = "sentinel2_l2a",
  roi = roi,              # Hier Region angeben
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  bands = c("B02", "B03", "B04", "B08"), # gewünschte Bänder
  multicore = 2,          # optional, parallele Verarbeitung
  progress = TRUE
)

################ openeo ##################


con <- connect("https://openeofed.dataspace.copernicus.eu")
con$login()

colls <- list_collections(con)
print(colls)

p <- processes()

data <- p$load_collection(
  id = "SENTINEL2_L2A",
  spatial_extent = list(west = 10, south = 50, east = 11, north = 51),
  temporal_extent = list("2018-04-22", "2018-04-22"),
  bands = c("B04", "B08")
)
# Weiterverarbeitung hier
# Wenn keine weitere Verarbeitung, dann data direkt speichern als GeoTIFF ZIP
formats <- list_file_formats(con)
result <- p$save_result(data = data, format = formats$output$`GTIFF-ZIP`)

# Job anlegen
job <- create_job(con, graph = result, title = "Sentinel-2 Download")

# Job starten
start_job(con, job$id)

# Jobstatus überwachen (kann in einer Schleife erfolgen)
while(TRUE) {
  status <- describe_job(con, job$id)$status
  print(status)
  if (status %in% c("finished", "failed", "error")) break
  Sys.sleep(5)
}

# Ergebnisse herunterladen (Ordner anpassen)
download_results(con, job$id, folder = "path/to/save")




result <- p$save_result(data, format = "GTiff")
download_results(result, folder = "Data/S2/RAW", con = con)

ls(package:openeo)
openeo::download_file()