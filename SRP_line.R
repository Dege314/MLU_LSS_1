packages <- c("ggplot2", "dplyr", "ggpubr")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}
# plotting Vegetation- Residue- and Soilline
data <- read.csv("Data/tables/extracted_values.csv") 

data_long <- pivot_longer(data, cols = -c(1,2), names_to = "band", values_to = "value")

ggplot(data[data$class == 3,], aes(x = B04, y = B08)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Scatterplot of B04 RED vs B08 NIR - Vegetation",
         x = "B04 RED",
         y = "B08 NIR") +
    theme_minimal()

# plotting lines
png("Data/plots/scatterplot_lines.png", width = 800, height = 600)
ggplot() +
    geom_point(data = data, aes(x = B04, y = B08, color = as.factor(class))) +
    labs(title = "Scatterplot of B04 RED vs B08 NIR - Visualization of Vegetation-, Soil-, and Residue-line concept",
         x = "B04 RED",
         y = "B08 NIR",
         color = "Class") +
    geom_smooth(data = data[data$class == 1,], aes(x = B04, y = B08), method = "lm", se = FALSE) +
    geom_smooth(data = data[data$class == 2,], aes(x = B04, y = B08), method = "lm", se = FALSE) +
    geom_smooth(data = data[data$class == 3,], aes(x = B04, y = B08), method = "lm", se = FALSE) +
    theme_minimal() +
    scale_color_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                       labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue"))
dev.off()
# plotting indices
ggplot() +
  geom_point(data = data[data$class != 3,], aes(x = BSI, y = RVI, color = as.factor(class))) +
  labs(title = "Scatterplot of RVI vs BSI",
       x = "BSI",
       y = "RVI",
       color = "Class") +
  theme_minimal() +
  scale_color_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                     labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue"))


ggplot() +
  geom_point(data = data[data$class != 3,], aes(y = NBR2, x = BSI, color = as.factor(class))) +
  labs(title = "Scatterplot of BSI vs NBR2",
       y = "NBR2",
       x = "BSI",
       color = "Class") +
  theme_minimal() +
  scale_color_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                     labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue"))

# plotting SAR

ggplot()+
    geom_point(data = data, aes(x = VV, y = VH, color = as.factor(class))) +
    labs(title = "Scatterplot of VH vs VV",
         x = "VV",
         y = "VH",
         color = "Class") +
    theme_minimal() +
    scale_color_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                       labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue"))


ggplot() +
  geom_line(data = data_long, aes(x = band, y = value, interaction(date, class), color = as.factor(class)), alpha = 0.3) +
  labs(title = "Spectral Profiles of Different Classes",
       x = "Wavelength Band",
       y = "Reflectance",
       color = "Class") +
  theme_minimal() +
  scale_color_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                     labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue"))

head(data)


ggplot(data_long, aes(x = band, y = value, group = interaction(date, class), color = as.factor(class))) +
  geom_line(alpha = 0.7) +
  labs(
    title = "Spectral Profiles of Different Classes",
    x = "Wavelength Band",
    y = "Reflectance",
    color = "Class"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("3" = "green", "1" = "red", "2" = "orange"),
    labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue")
  )


data_long <- data %>%
  mutate(row_id = row_number(),
         VH = VH*-200,
         VV = VV*-200,
         RVI = (VV / VH)*5000) %>%
  pivot_longer(cols = -c(date, class, row_id), names_to = "band", values_to = "value")

data <- data %>%
  mutate(row_id = row_number(), 
         RVI = (VV / VH),
         BSI = ((B11 + B04) - (B08 + B02)) / ((B11 + B04) + (B08 + B02)))
          %>%
  pivot_longer(cols = -c(date, class, row_id), names_to = "band", values_to = "value")

ggplot(data_long, aes(x = band, y = value, group = row_id, color = as.factor(class))) +
  geom_line(alpha = 0.7) +
  labs(
    title = "Spectral Profiles of Different Classes",
    x = "Wavelength Band",
    y = "Reflectance",
    color = "Class"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("3" = "green", "1" = "red", "2" = "orange"),
    labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue")
)

ggplot(data_long[data_long$band %in% c("VH", "VV", "RVI"), ], aes(x = band, y = value, group = row_id, color = as.factor(class))) +
  geom_line(alpha = 0.5) +
  labs(
    title = "Spectral Profiles of Different Classes",
    x = "Wavelength Band",
    y = "Reflectance",
    color = "Class"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("3" = "green", "1" = "red", "2" = "orange"),
    labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue")
)
str(data)
dates <- unique(data$date)
plot_list <- list()

for (date in dates) {
    p <- ggplot(data[data$date == date, ], aes(x = BSI, y = RVI, color = as.factor(class))) +
        geom_point() +
        labs(title = paste("Scatterplot of BSI vs RVI on", date),
             x = "BSI", y = "RVI", color = "Class") +
        theme_minimal() +
        scale_color_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                            labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue"))
    plot_list[[as.character(date)]] <- p
}

png("Data/plots/scatterplots_BSI_RVI.png", width = 800, height = 600)
# Plots mit ggarrange anordnen, z.B. 2 Spalten
ggarrange(plotlist = plot_list, ncol = 3, nrow = ceiling(length(plot_list)/3))
dev.off()

soils <- c("TT", "RZ")
# Boxplots per Date
unique(data$BODTYP_K)
for (date in dates) {
    #subset data by timestamp
    data_date_sub <- data[data$date == date, ]
    for (soil in soils) {
        data_sub <- data_date_sub[data_date_sub$BODTYP_K == soil, ]
    #create filename
    png(paste0("Data/plots/boxplots_Indices_", date, "_", soil, ".png"), width = 800, height = 600)
    # Boxplots
    p1 <- ggplot(data_sub, aes(x = as.factor(class), y = BSI, fill = as.factor(class))) +
    geom_boxplot() +
    labs(title = "Boxplot of BSI by Class",
        x = "Class",
        y = "BSI",
        fill = "Class") +
    theme_minimal() +
    scale_fill_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                        labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue"))
    p2 <- ggplot(data_sub, aes(x = as.factor(class), y = RVI, fill = as.factor(class))) +
    geom_boxplot() +
    labs(title = "Boxplot of RVI by Class",
        x = "Class",
        y = "RVI",
        fill = "Class") +
    theme_minimal() +
    scale_fill_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                        labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue"))
    p3 <- ggplot(data_sub, aes(x = as.factor(class), y = NBR2, fill = as.factor(class))) +
    geom_boxplot() +
    labs(title = "Boxplot of NBR2 by Class",
        x = "Class",
        y = "NBR2",
        fill = "Class") +
    theme_minimal() +
    scale_fill_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                        labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue"))
    p4 <- ggplot(data_sub, aes(x = as.factor(class), y = VV, fill = as.factor(class))) +
    geom_boxplot() +
    labs(title = "Boxplot of VV by Class",
        x = "Class",
        y = "VV",
        fill = "Class") +
    theme_minimal() +
    scale_fill_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                        labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue"))
    p5 <- ggplot(data_sub, aes(x = as.factor(class), y = VH, fill = as.factor(class))) +
    geom_boxplot() +
    labs(title = "Boxplot of VH by Class",
        x = "Class",
        y = "VH",
        fill = "Class") +
    theme_minimal() +
    scale_fill_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                        labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue"))


    combined <- ggarrange(p1, p2, p3, p4, p5, ncol = 3, nrow = 2)
    final_plot <- annotate_figure(combined, top = paste("Boxplots for", date))
    print(final_plot)
    dev.off()
  }
}
cor(data$RVI, data$Ratio, method = "pearson", use = "complete.obs")
str(data_sub)
