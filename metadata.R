packages <- c("xml2", "ggplot2")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}
ref_data <- terra::vect("data/ref_data.gpkg") %>% project("EPSG:32632") # <- crs der Sentinel-2 Daten
soil_data <- terra::vect("Data/vector/vbk/vbk50_2009_salza_fixed.shp") %>% project("EPSG:32632") # <- crs der Sentinel-2 Daten
soil_data <- soil_data[, "BODTYP_K"]

# Bodentypen extrahieren und zusammenfassen
ref_data <- terra::intersect(ref_data, soil_data[, "BODTYP_K"])
# Bodentypen mit weniger als 150 Vorkommen zu "Other" zusammenfassen
soiltypes <- as.data.frame(ref_data) %>%
  group_by(BODTYP_K) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
types_to_aggregate <- soiltypes$BODTYP_K[soiltypes$count < 150]

values(ref_data) <- as.data.frame(ref_data) %>%
  mutate(BODTYP_K = ifelse(BODTYP_K %in% types_to_aggregate, "Other", BODTYP_K))

writeVector(ref_data, "data/ref_data_soil_merge_32632.gpkg", overwrite = TRUE)
soiltypes_aggr <- as.data.frame(ref_data) %>%
  group_by(BODTYP_K) %>%
  summarise(bf_masking = n()) %>%
  arrange(desc(bf_masking))
write.csv(soiltypes_aggr, "data/tables/soiltypes_aggregated.csv", row.names = FALSE)

# extract S1 meta data
meta_data <- data.frame()

for (i in seq_along(S1_zipfiles)) {

# create path
xml_path <- paste0(substr(basename(S1_zipfiles[1]), 1, 67),
                   ".SAFE/annotation/",
                   tolower(substr(basename(S1_zipfiles[1]), 1, 3)),
                   "-iw-grd-vh-",
                   gsub("_", "-", tolower(substr(basename(S1_zipfiles[1]), 18, 62))),
                   "-002.xml")
                   
meta_xml <- unz(S1_zipfiles[1], xml_path)
doc <- read_xml(meta_xml)
md <- data.frame(
  file_name = basename(S1_zipfiles[i]),
  date = as.Date(substr(basename(S1_zipfiles[i]), 18, 25), format = "%Y%m%d"),
  pass = xml_text(xml_find_first(doc, ".//pass"))
)
meta_data <- rbind(meta_data, md)
}
View(meta_data)
write.csv(meta_data, "data/tables/S1_metadata.csv", row.names = FALSE)

S2_zipfiles <- list.files("Data/S2/RAW", pattern = "\\.zip$", full.names = TRUE)
S2_dates <- data.frame(date = as.Date(substr(basename(S2_zipfiles), 12, 19), format = "%Y%m%d"))

write.csv(S2_dates, "data/tables/S2_dates.csv", row.names = FALSE)

ref_data <- as.data.frame(vect("data/ref_data.gpkg")) %>% mutate(date = as.Date(date, format = "%Y-%m-%d"))

#Plot dates
ggplot() +
  geom_point(data = meta_data, aes(x = date, y = 1)) +
  geom_point(data = ref_data, aes(x = date, y = 2), color = "red") +
  geom_point(data = S2_dates, aes(x = date, y = 3), color = "blue")

str(meta_data)
str(S2_dates)
str(ref_data)
