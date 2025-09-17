library(car)
library(ggplot2)

data <- read.csv("Data/tables/extracted_values.csv", header = TRUE, stringsAsFactors = FALSE)
str(data)
head(data)
# Klassen auf Normalverteilung prüfen

shapiro.test(data$VH_mean[data$class == 1])
shapiro.test(data$VH_mean[data$class == 2])
shapiro.test(data$VH_mean[data$class == 3])
hist(data$VH_mean[data$class == 1])
hist(data$VH_mean[data$class == 2])
hist(data$VH_mean[data$class == 3])
# Die Daten sind nicht normalverteilt

leveneTest(VH_mean ~ as.factor(class), data = data)
# Die Varianzen sind nicht homogen


# müssen testen, ob Klassen aus derselben Grundgesamtheit stammen (Kruskal-Wallis-Test)

# Beispiel an VH_mean ------------------------------------------------------------
# Kruskal Test beispielhaft für VH_mean
kruskal.test(VH_mean ~ as.factor(class), data = data)
# --> Nullhypothese (alle drei Klassen stammen aus einer Grundgesamtheit) klar verworfen; 
# mindestens eine Klasse unterscheidet sich signifikant von den anderen

# nächster Schritt: Post-hoc-Test, um herauszufinden, zwischen welchen Klassen Unterschiede in der Grundgesamtheit bestehen. 

library(rstatix)

# Posthoc Dunn-Test
dunn_result <- dunn_test(data, VH_mean ~ class, p.adjust.method = "bonferroni")
dunn_result

# Ergebnisse sind stark signifikant. 

# Testen im folgenden für alle Textur- und Indexvariablen, welche Variablen zwischen den KLassen signifikant unterschiedlich sind. 


# Alle Variablen auf signifikante Unterschiede testen ---------------------------
# 1️ Texturparameter
texture_cols <- c(
  "VH_mean", "VH_variance", "VH_homogeneity", "VH_contrast", "VH_dissimilarity",
  "VH_entropy", "VH_second_moment", "VH_correlation",
  "VV_mean", "VV_variance", "VV_homogeneity", "VV_contrast", "VV_dissimilarity",
  "VV_entropy", "VV_second_moment", "VV_correlation"
)

# 2 Andere numerische Spalten (Bänder + Indizes)
all_numeric <- names(data)[!(names(data) %in% c("date", "class", "row_id"))]
other_cols <- setdiff(all_numeric, texture_cols)


library(dplyr)
library(rstatix)

# Sicherstellen, dass class ein Faktor ist
data$class <- as.factor(data$class)

# Alle Variablen, die getestet werden sollen
all_vars <- c(texture_cols, other_cols)

# Funktion, um Kruskal-Wallis + Dunn-Test für alle Variablen zu machen
run_all_kruskal_dunn <- function(vars, df) {
  results_list <- list()
  
  for (var in vars) {
    # Kruskal-Wallis-Test
    kw <- kruskal_test(df, formula = as.formula(paste(var, "~ class")))
    
    # Dunn-Posthoc, falls signifikant
    if (kw$p < 0.05) {
      dunn <- dunn_test(df, formula = as.formula(paste(var, "~ class")), p.adjust.method = "bonferroni")
      dunn_summary <- dunn %>%
        select(.y., group1, group2, p.adj, p.adj.signif)
    } else {
      dunn_summary <- tibble(.y. = var,
                             group1 = NA,
                             group2 = NA,
                             p.adj = NA,
                             p.adj.signif = NA)
    }
    
    results_list[[var]] <- dunn_summary
  }
  
  # Alle Ergebnisse in einem großen Dataframe zusammenfassen
  results_df <- bind_rows(results_list)
  return(results_df)
}

# Ausführen
all_results <- run_all_kruskal_dunn(all_vars, data)

# Übersicht anzeigen
View(all_results)

# Als CSV speichern
write.csv(all_results, "Data/Ergebnisse/kruskal_dunn_results.csv", row.names = FALSE)



# kompakte Tabelle mit signifikanten Variablen erstellen ------------------------

library(dplyr)
library(rstatix)
library(tidyr)
library(tibble)

run_dunn_compact_pairs <- function(vars, df) {
  results_list <- list()
  
  for (var in vars) {
    kw <- kruskal_test(df, formula = as.formula(paste(var, "~ class")))
    
    if (kw$p < 0.05) {
      dunn <- dunn_test(df, formula = as.formula(paste(var, "~ class")), p.adjust.method = "bonferroni") %>%
        mutate(
          Variable = var,
          KW_p = kw$p,
          KW_signif = "Yes",
          pair = paste0(group1, "_vs_", group2),
          p_adj = p.adj,
          signif = p.adj.signif,
          signif_flag = ifelse(p.adj < 0.05, "Yes", "No")
        ) %>%
        select(Variable, KW_p, KW_signif, pair, p_adj, signif, signif_flag)
      
    } else {
      # Wenn KW nicht signifikant: lege Dummy-Zeilen für alle 3 Paare an (NA p, "No" flags)
      pairs <- c("1_vs_2", "1_vs_3", "2_vs_3")
      dunn <- tibble(
        Variable = var,
        KW_p = kw$p,
        KW_signif = "No",
        pair = pairs,
        p_adj = as.numeric(NA),
        signif = NA_character_,
        signif_flag = "No"
      )
    }
    
    results_list[[var]] <- dunn
  }
  
  results_long <- bind_rows(results_list)
  
  # wide format: p_adj_1_vs_2, signif_1_vs_2, signif_flag_1_vs_2 usw.
  results_wide <- results_long %>%
    pivot_wider(names_from = pair,
                values_from = c(p_adj, signif, signif_flag),
                names_sep = "_")
  
  # kompakte Spalte mit den wirklich signifikanten Paaren
  sig_cols <- c("signif_flag_1_vs_2", "signif_flag_1_vs_3", "signif_flag_2_vs_3")
  pair_names <- c("1_vs_2", "1_vs_3", "2_vs_3")
  # Falls die Spalten fehlen (z. B. andere Klassenbeschriftung), sichere Abfrage:
  present_sig_cols <- intersect(sig_cols, names(results_wide))
  if (length(present_sig_cols) == length(pair_names)) {
    results_wide$signif_pairs <- apply(results_wide[present_sig_cols], 1, function(x) {
      paste(pair_names[which(x == "Yes")], collapse = "; ")
    })
    results_wide$signif_pairs[results_wide$signif_pairs == ""] <- "none"
  } else {
    # Fallback: wenn Paar-Spalten anders benannt sind (z.B. "signif_flag_Soil_vs_Residue"), setze NA
    results_wide$signif_pairs <- NA_character_
  }
  return(list(long = results_long, wide = results_wide))
}

# Anwendung und Export
out <- run_dunn_compact_pairs(all_vars, data)
View(out)
all_results_long <- out$long
all_results_wide <- out$wide

# Speichern
write.csv(all_results_long, "Data/Ergebnisse/kruskal_dunn_pairs_long.csv", row.names = FALSE)
write.csv(all_results_wide, "Data/Ergebnisse/kruskal_dunn_pairs_wide.csv", row.names = FALSE)


# Nur signifikante Werte über alle Klassen
signif_classes<- all_results_wide %>% filter(signif_flag_1_vs_2 == "Yes")
View(signif_classes)

# Nur Soil vs Residue (1_vs_2) signifikant
soil_vs_residue <- all_results_wide %>%
  filter(signif_flag_1_vs_2 == "Yes") %>%
  select(Variable, KW_p, KW_signif,
         p_adj_1_vs_2, signif_1_vs_2, signif_flag_1_vs_2)



# Plotten aller Variablen nach class getrennt -----------------------------------

library(ggplot2)
library(rstatix)
library(dplyr)


# 1) signifikante Texturparameter plotten ---------------------------------------
# (sehen fehlerhaft oder unbrauchbar aus)

signif_vars <- soil_vs_residue$Variable
signif_vars <- signif_vars[signif_vars %in% names(data)]  # nur Spalten, die wirklich existieren

# 2) Texture-Parameter filtern auf die signifikanten Variablen
texture_sig <- intersect(texture_cols, signif_vars)
texture_long <- data %>%
  select(class, all_of(texture_sig)) %>%
  pivot_longer(cols = all_of(texture_sig), names_to = "Variable", values_to = "Value")

# Plot nur für signifikante Texture-Parameter
ggplot(texture_long, aes(x = class, y = Value, fill = class)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Significant Texture Parameters by Class",
       x = "Class", y = "Value") +
  scale_fill_manual(values = c("3"="green","1"="red","2"="orange"),
                    labels = c("3"="Vegetation","1"="Soil","2"="Residue")) +
  theme_minimal()


summary(data[texture_sig])


ggplot(texture_long, aes(x = class, y = Value, fill = class)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_y") +
  scale_y_log10() +   # für Parameter mit kleinen Werten
  labs(title = "Significant Texture Parameters by Class (log scale)",
       x = "Class", y = "Value") +
  scale_fill_manual(values = c("3"="green","1"="red","2"="orange"),
                    labels = c("3"="Vegetation","1"="Soil","2"="Residue")) +
  theme_minimal()



# nur die Klassen 1 und 2
data_subset <- data %>% filter(class %in% c("1","2"))

texture_sig <- intersect(texture_cols, soil_vs_residue$Variable)

texture_long <- data_subset %>%
  select(class, all_of(texture_sig)) %>%
  pivot_longer(cols = all_of(texture_sig), names_to = "Variable", values_to = "Value")

ggplot(texture_long, aes(x = class, y = Value, fill = class)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Significant Texture Parameters (Soil vs Residue)",
       x = "Class", y = "Value") +
  scale_fill_manual(values = c("1"="red","2"="orange"),
                    labels = c("1"="Soil","2"="Residue")) +
  theme_minimal()




# 3) alle anderen Variablen aus other_cols plotten ------------------------------
#dieser Plot sollte stimmen. Probleme bei texturparameter

other_sig <- intersect(other_cols, signif_vars)
other_sig_num <- other_sig[sapply(data[other_sig], is.numeric)]  # nur numerische Spalten

other_long <- data %>%
  select(class, all_of(other_sig_num)) %>%
  pivot_longer(cols = all_of(other_sig_num), names_to = "Variable", values_to = "Value")

# Plot nur für signifikante andere Variablen
ggplot(other_long, aes(x = class, y = Value, fill = class)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Significant Band Indices and Other Variables by Class",
       x = "Class", y = "Value") +
  scale_fill_manual(values = c("3"="green","1"="red","2"="orange"),
                    labels = c("3"="Vegetation","1"="Soil","2"="Residue")) +
  theme_minimal()
