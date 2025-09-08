packages <- c("terra", "tidyr", "dplyr", "openxlsx", "sf")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}
ref_data <- terra::vect("data/ref_data.gpkg")
dates <- unique(ref_data$date)
classes <- unique(ref_data$class)
crs(ref_data)
# count number of points per date and class
counts <- data.frame(date = character(), class = character(), count = integer())
for (date in dates) {
  for (class in classes) {
    count <- nrow(ref_data[ref_data$date == date & ref_data$class == class, ])
    counts <- rbind(counts, data.frame(date = date, class = class, count = count))
  }
}
# ins wide formtat
counts_table <- pivot_wider(counts, names_from = date, values_from = count, values_fill = 0)

# sortieren
counts_table <- counts_table %>%
  arrange(class)

# Summen berechnen
counts_table$total <- rowSums(counts_table[ , -1], na.rm = TRUE)
col_sums <- colSums(counts_table[ , -1], na.rm = TRUE)

# Erstelle eine neue Datenzeile mit den Summen der Spalten
total_row <- c(class = "Summe", as.list(colSums(counts_table[ , -1], na.rm = TRUE)))

# Füge sie an counts_table an (umwandeln in Data Frame mit einem Row-Bind)
counts_table <- rbind(counts_table, total_row)

write.csv(counts_table, "data/tables/ref_data_counts.csv", row.names = FALSE)
write.xlsx(counts_table, "data/tables/ref_data_counts.xlsx", rowNames = FALSE)

e <- ext(ref_data)
WKT <- paste0("POLYGON((",
           e[1], " ", e[3], ", ",
           e[1], " ", e[4], ", ",
           e[2], " ", e[4], ", ",
           e[2], " ", e[3], ", ",
           e[1], " ", e[3], "))")
writeLines(WKT, "data/tables/ref_data_extent_epsg4326.wkt")

poly <- as.polygons(e, crs = crs(ref_data))

# Buffer hinzufügen
poly_buffered <- buffer(poly, 30)

#extent erzeugen
e_buff <- ext(poly_buffered)

# WKT ausgeben
WKT_buffered <- paste0("POLYGON((",
                       e_buff[1], " ", e_buff[3], ", ",
                       e_buff[1], " ", e_buff[4], ", ",
                       e_buff[2], " ", e_buff[4], ", ",
                       e_buff[2], " ", e_buff[3], ", ",
                       e_buff[1], " ", e_buff[3], "))")

writeLines(WKT_buffered, "data/tables/ref_data_extent_epsg4326_buff30m.wkt")

# WKT für EPSG 32632
poly_buffered_32632 <- st_transform(st_as_sf(poly_buffered), crs = 32632)

e_buff <- ext(poly_buffered_32632)

WKT_buffered_32632 <- paste0("POLYGON((",
                       e_buff[1], " ", e_buff[3], ", ",
                       e_buff[1], " ", e_buff[4], ", ",
                       e_buff[2], " ", e_buff[4], ", ",
                       e_buff[2], " ", e_buff[3], ", ",
                       e_buff[1], " ", e_buff[3], "))")

writeLines(WKT_buffered_32632, "data/tables/ref_data_extent_epsg32632_buff30m.wkt")
