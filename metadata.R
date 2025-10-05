packages <- c("xml2", "ggplot2", "terra", "dplyr", "ggpubr")
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

# cloud mask counting and plot
tif_files <- list.files("Data/S2/pro/mosaic", pattern = "\\.tif$", full.names = TRUE)
plot(rast(tif_files[1]))
rast(tif_files[1])

cloud_count <- data.frame()
plot_list <- list()
for (i in seq_along(tif_files)) {
  cat(sprintf("\rCounting and plotting - progress: %d of %d (%s)", i, length(tif_files), basename(tif_files[i])))
  img <- rast(tif_files[i])
  total_pixels <- ncell(img[[1]])
  na_pixels <- sum(is.na(values(img[[1]])))
  non_na_pixels <- total_pixels - na_pixels
  na_percent <- (na_pixels / total_pixels) * 100
  date <- format(as.Date(substr(basename(tif_files[i]), 12, 19), format = "%Y%m%d"), "%Y-%m")
  cloud_data <- data.frame(date = date, total = total_pixels, na = na_pixels, non_na = non_na_pixels, na_percent = na_percent)
  # create plot
  image_df <- as.data.frame(img$B8A, xy=TRUE)
  plot_list[[i]] <- ggplot(image_df, aes(x = x, y = y)) +
                           geom_raster() +
                           scale_fill_viridis_c() +
                           ggtitle(paste0(date, " - Cloud-Mask: ", round(na_percent, 1), "%"))
  if (i == 1) {
    cloud_count <- cloud_data
  } else {
    cloud_count <- rbind(cloud_count, cloud_data)
  }
  if (i == length(tif_files)) {
    final_plot <- do.call(ggarrange, c(plot_list, list(ncol = 3, nrow = 3)))
    ggsave("Data/plots/cloud_masks.png", final_plot, width = 15, height = 15)
    write.csv(cloud_count, "data/tables/S2_cloud_percentage_per_date.csv", row.names = FALSE)
    cat("\rCounting finished!                                                      \n")
  }
}
View(cloud_count)
sum(is.na(values(rast(tif_files[[1]]))))
sum(!is.na(values(rast(tif_files[[1]]))))

combined <- do.call(ggarrange, c(p_list, list(ncol = 4, nrow = ceiling(length(p_list)/4), common.legend = TRUE, legend = "right")))

final_plot <- annotate_figure(combined, top = text_grob("Boxplots of the best distinguishable variables per class", size = 14, face = "bold"))

plot(plot_list[[1]])
str(plot_list[[1]])

png("Data/plots/cloud_masks.png", width = 15, height = 15, units = "in", res = 300)
par(mfrow = c(3, 3),
    mai = c(0.6, 0.6, 0.8, 0.2)) # margins: bottom, left, top, right
cloud_count <- data.frame()
plot_list <- list()
for (i in seq_along(tif_files)) {
  cat(sprintf("\rCounting and plotting - progress: %d of %d (%s)", i, length(tif_files), basename(tif_files[i])))
  img <- rast(tif_files[i])
  total_pixels <- ncell(img[[1]])
  na_pixels <- sum(is.na(values(img[[1]])))
  non_na_pixels <- total_pixels - na_pixels
  na_percent <- (na_pixels / total_pixels) * 100
  date <- format(as.Date(substr(basename(tif_files[i]), 12, 19), format = "%Y%m%d"), "%Y-%m")
  cloud_data <- data.frame(date = date, total = total_pixels, na = na_pixels, non_na = non_na_pixels, na_percent = na_percent)
  # create plot
  #plot(img$B8A)
  plotRGB(img, r=3, g=2, b=1, stretch="lin", main=paste0(date, " - Cloud-Mask: ", round(na_percent, 1), "%"), axes = TRUE)
  add_grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = 1)
    if (i == 1) {
    cloud_count <- cloud_data
  } else {
    cloud_count <- rbind(cloud_count, cloud_data)
  }
  if (i == length(tif_files)) {
    write.csv(cloud_count, "data/tables/S2_cloud_percentage_per_date.csv", row.names = FALSE)
    cat("\rCounting finished!                                                      \n")
    dev.off()
  }
}
