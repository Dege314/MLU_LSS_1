packages <- c("ggplot2", "dplyr", "ggpubr")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}
# plotting Vegetation- Residue- and Soilline
variables <- readLines("Data/tables/best_RF_performing_variables.txt")
data <- read.csv("Data/tables/extracted_values.csv") 
data <- data %>%
  mutate(class_2 = case_when(
    class == 1 ~ 1,
    class == 2 ~ 2,
    class == 3 ~ 2
  ))

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
    theme(plot.title = element_text(size = 14, face = "bold")) +
    scale_color_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                       labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue"))
dev.off()
# SAR plots
png("Data/plots/scatterplot_RVI_buff_vs_VH_variance.png", width = 800, height = 600)
ggplot()+
    geom_point(data = data, aes(x = RVI_buff, y = VH_variance, color = as.factor(class))) +
    labs(title = "Scatterplot of RVI buff vs VH variance",
         y = "VH variance",
         x = "RVI buff",
         color = "Class") +
    theme_minimal() +
    scale_color_manual(values = c("3" = "green", "1" = "red", "2" = "blue"),
                       labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue"))
dev.off()
# Scatterplots NBR" RVI buff
  png(paste0("Data/plots/scatterplot_NBR2_RVI_buff.png"), width = 800, height = 600)
    ggplot(data, aes(x = NBR2, y = RVI_buff, color = as.factor(class))) +
        geom_point() +
        labs(title = paste("Scatterplot of NBR2 vs RVI_buff on", date),
             x = "NBR2", y = "RVI_buff", color = "Class") +
        theme_minimal() +
        scale_color_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                            labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue"))
    dev.off()

png("Data/plots/scatterplots_NBR2_RVI_buff.png", width = 800, height = 600)
# Plots mit ggarrange anordnen, z.B. 2 Spalten
ggarrange(plotlist = plot_list, ncol = 3, nrow = ceiling(length(plot_list)/3))
dev.off()

## Boxplots erstellen
### Boxplots aller Variablen über alle Zeitpunkte
# Boxplot it 3 Klassen
png(paste0("Data/plots/boxplots_best_variables_over_all_dates_by_3_classes.png"), width = 800, height = 600)
  p_list <- list()
for (name in variables) {
  p <- ggplot(data, aes(x = as.factor(class), y = !!sym(name), fill = as.factor(class))) +
    geom_boxplot() +
    labs(title = paste(name),
         x = "Class",
         y = name,
         fill = "Class") +
    theme_minimal() +
    scale_fill_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                      labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue")) +
    theme(legend.position = "none")   # Legende ausblenden

  p_list[[paste0("p", name)]] <- p
}
p_list[[1]] <- p_list[[1]] + theme(legend.position = "right")  # Legende nur hier aktiv

combined <- do.call(ggarrange, c(p_list, list(ncol = 4, nrow = ceiling(length(p_list)/4), common.legend = TRUE, legend = "right")))

final_plot <- annotate_figure(combined, top = text_grob("Boxplots of the best distinguishable variables per class", size = 14, face = "bold"))
plot(final_plot)
dev.off()

# Boxplot it 2 Klassen
png(paste0("Data/plots/boxplots_best_variables_over_all_dates_by_2_classes.png"), width = 800, height = 600)
  p_list <- list()
for (name in variables) {
  p <- ggplot(data, aes(x = as.factor(class_2), y = !!sym(name), fill = as.factor(class_2))) +
    geom_boxplot() +
    labs(title = paste(name),
         x = "Class",
         y = name,
         fill = "Class") +
    theme_minimal() +
    scale_fill_manual(values = c("3" = "green", "1" = "red", "2" = "orange"),
                      labels = c("3" = "Vegetation", "1" = "Soil", "2" = "Residue")) +
    theme(legend.position = "none")   # Legende ausblenden

  p_list[[paste0("p", name)]] <- p
}
p_list[[1]] <- p_list[[1]] + theme(legend.position = "right")  # Legende nur hier aktiv

combined <- do.call(ggarrange, c(p_list, list(ncol = 4, nrow = ceiling(length(p_list)/4), common.legend = TRUE, legend = "right")))

final_plot <- annotate_figure(combined, top = text_grob("Boxplots of the best distinguishable variables per class", size = 14, face = "bold"))
plot(final_plot)
dev.off()