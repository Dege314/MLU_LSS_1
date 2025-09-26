packages <- c("randomForest", "splitTools", "dplyr", "stringr", "purrr", "openxlsx")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}

# load data
data <- read.csv("Data/tables/extracted_values.csv")
dates <- unique(data$date)

variables <- readLines(r"(Data\tables\best_RF_performing_variables.txt)")
# subset data by best variables
data <- data %>%
  dplyr::select(class, BODTYP_K, date, all_of(variables))
# count Nas
colSums(is.na(data))
data <- na.omit(data)
# split Dataset into different sets of timepoints and variable combinations
n_class <- 2 # 2 for 2-class, 3 for 3-class
if (n_class == 2) {
  data$class <- ifelse(data$class == 3, 2, data$class)
}
# für jede Variable einzeln testen
overall_accuracy_var <- data.frame(
  set = character(),
  OA_accurracy = numeric(),
  OOB_error = numeric(),
  stringsAsFactors = FALSE
)
for (var in variables) {
  set <- data[, c("class", "BODTYP_K", "date", var)]
  # Nur die Spalten auswählen, die für die Stratifikation genutzt werden sollen
  strata_data <- set[, c("class", "BODTYP_K", "date")]

  # multi_strata erzeugt die Schichten (Strata)
  strata_factor <- multi_strata(strata_data, strategy = "interaction")

  # Daten partitionieren mit der stratification
  set.seed(161)
  partitions <- partition(strata_factor, p = c(train = 0.8, valid = 0.2))
  train_data <- set[partitions$train, -which(names(set) %in% c("date", "BODTYP_K"))]
  valid_data <- set[partitions$valid, ]
  train_data$class <- as.factor(train_data$class)
  set.seed(161)
  rf_model <- randomForest(class ~ ., data = train_data, ntree = 500)
  prediction <- predict(rf_model, newdata = valid_data)
  validation <- data.frame(prediction = as.numeric(prediction), actual = as.numeric(valid_data$class))
  OA <- sum(validation$prediction == validation$actual) / nrow(validation)
  OOB <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]

  overall_accuracy_var <- rbind(overall_accuracy_var, data.frame(set = var, OA_accurracy = OA, OOB_error = OOB))
}
overall_accuracy_var <- arrange(overall_accuracy_var, desc(OA_accurracy))

write.csv(overall_accuracy_var, "Data/tables/random_forest_overall_accuracy_per_variable_2class.csv", row.names = FALSE)
# test for different variable combinations
# Liste aller Variablen-Kombinationen erstellen
var_comb <- list()
for (k in 1:length(variables)) {
  comb_k <- combn(variables, k, simplify = FALSE)
  var_comb <- c(var_comb, comb_k)
}

# Schleife über alle sets mit und ohne Datum als Variable
overall_accuracy_all <- data.frame(set = seq(1, length(var_comb)))
use_date <- c("TRUE", "FALSE")
for(option in use_date) {
  # Schleife über alle Sets
  overall_accuracy <- data.frame(
    set = character(),
    all_timepoints = numeric(),
    # oob_error = numeric(),
    # variables = character(),
    stringsAsFactors = FALSE
  )
  integer <- 1
  for (names in var_comb) {
    cat(sprintf("\rprocessing set: %d of %d in %s", integer, length(var_comb), ifelse(option == "TRUE", "first loop (with date)", "second loop (without date)")))
    set <- data %>% dplyr::select(class, BODTYP_K, date, all_of(names))
    #head(set)
    # Nur die Spalten auswählen, die für die Stratifikation genutzt werden sollen
    strata_data <- set[, c("class", "BODTYP_K", "date")]

    # multi_strata erzeugt die Schichten (Strata)
    strata_factor <- multi_strata(strata_data, strategy = "interaction")

    # Daten partitionieren mit der stratification
    set.seed(161)
    partitions <- partition(strata_factor, p = c(train = 0.8, valid = 0.2))
    if (option == "FALSE") {
      train_data <- set[partitions$train, -which(names(set) %in% c("date"))]
    } else if (option == "TRUE") {
      train_data <- set[partitions$train, ]
    }
    valid_data <- set[partitions$valid, ]
    train_data$class <- as.factor(train_data$class)
    set.seed(161)
    rf_model <- randomForest(class ~ ., data = train_data, ntree = 500)
    prediction <- predict(rf_model, newdata = valid_data)
    validation <- data.frame(prediction = as.numeric(prediction), actual = as.numeric(valid_data$class))
    OA <- sum(validation$prediction == validation$actual) / nrow(validation)
    OOB <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]

    overall_accuracy <- rbind(overall_accuracy, data.frame(set = integer, all_timepoints = OA))
    integer <- integer + 1
  }
  if (option == "FALSE") {
    names(overall_accuracy)[names(overall_accuracy) == "all_timepoints"] <- paste("OA_no_date")
  } else if (option == "TRUE") {
    names(overall_accuracy)[names(overall_accuracy) == "all_timepoints"] <- paste("OA_with_date")
  }
  overall_accuracy_all <- merge(overall_accuracy_all, overall_accuracy, by = "set")
}
overall_accuracy_all$variables <- sapply(var_comb, function(x) paste(x, collapse = ", "))
overall_accuracy_all <- arrange(overall_accuracy_all, set)

## split by optical and SAR combined
optical_vars <- c("B8A", "MSAVI8A", "MSAVI2", "BSI8A", "NBR2")
sar_vars     <- c("RVI_buff", "VH_mean", "VH_variance")

overall_accuracy_all <- overall_accuracy_all %>%
  mutate(
    set_name = map_chr(
      str_split(variables, ",\\s*"),
      ~ if (all(.x %in% optical_vars)) {
          "optical"
        } else if (all(.x %in% sar_vars)) {
          "SAR"
        } else {
          "mixed"
        }
    )
  )
write.csv(overall_accuracy_all, "Data/tables/random_forest_overall_accuracy_all_sets_2class.csv", row.names = FALSE)

## choose the best performing sets
best_sets <- arrange(bind_rows(
  overall_accuracy_all %>%
    filter(set_name == "optical") %>%
    arrange(desc(OA_with_date)) %>%
    slice(1),
    
  overall_accuracy_all %>%
    filter(set_name == "SAR") %>%
    arrange(desc(OA_with_date)) %>%
    slice(1),
    
  overall_accuracy_all %>%
    filter(set_name == "mixed") %>%
    arrange(desc(OA_with_date)) %>%
    slice(1)
), desc(OA_with_date))

# Liste der drei besten Sets erstellen
set_list_all <- list()
for (i in 1:nrow(best_sets)) {
  set <- best_sets[i, ]
  set_list_all[[set$set_name]] <- data %>% dplyr::select(class, BODTYP_K, date, all_of(unlist(str_split(set$variables, ",\\s*"))))
}

# Schleife über alle besten Sets mit und ohne Datum als Variable
overall_accuracy <- data.frame(set = as.character(), all_timepoints = as.numeric())
user_accuracy_soil <- data.frame(set = as.character(), all_timepoints = as.numeric())
user_accuracy_veg <- data.frame(set = as.character(), all_timepoints = as.numeric())
producer_accuracy_soil <- data.frame(set = as.character(), all_timepoints = as.numeric())
producer_accuracy_veg <- data.frame(set = as.character(), all_timepoints = as.numeric())

for (set_name in names(set_list_all)) {
  set <- set_list_all[[set_name]]
  #cat(sprintf("processing set: %d \n", set))
  start_time <- Sys.time()
  #head(set)
  # Nur die Spalten auswählen, die für die Stratifikation genutzt werden sollen
  strata_data <- set[, c("class", "BODTYP_K", "date")]

  # multi_strata erzeugt die Schichten (Strata)
  strata_factor <- multi_strata(strata_data, strategy = "interaction")

  # Daten partitionieren mit der stratification
  set.seed(161)
  partitions <- partition(strata_factor, p = c(train = 0.8, valid = 0.2))
  if (option == "FALSE") {
    train_data <- set[partitions$train, -which(names(set) %in% c("date"))]
  } else if (option == "TRUE") {
    train_data <- set[partitions$train, ]
  }
  valid_data <- set[partitions$valid, ]
  train_data$class <- as.factor(train_data$class)
  set.seed(161)
  rf_model <- randomForest(class ~ ., data = train_data, ntree = 500)
  prediction <- predict(rf_model, newdata = valid_data)
  validation <- data.frame(prediction = as.numeric(prediction), actual = as.numeric(valid_data$class))
  OA <- sum(validation$prediction == validation$actual) / nrow(validation)
  OOB <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
  UA <- diag(table(validation)) / rowSums(table(validation))
  PA <- diag(table(validation)) / colSums(table(validation))

  overall_accuracy <- rbind(overall_accuracy, data.frame(set = set_name, all_timepoints = OA))
  user_accuracy_soil <- rbind(user_accuracy_soil, data.frame(set = set_name, all_timepoints = UA[1]))
  user_accuracy_veg <- rbind(user_accuracy_veg, data.frame(set = set_name, all_timepoints = UA[2]))
  producer_accuracy_soil <- rbind(producer_accuracy_soil, data.frame(set = set_name, all_timepoints = PA[1]))
  producer_accuracy_veg <- rbind(producer_accuracy_veg, data.frame(set = set_name, all_timepoints = PA[2]))
  end_time <- Sys.time()
  print(end_time - start_time)
}
overall_accuracy_strata <- overall_accuracy
user_accuracy_soil_strata <- user_accuracy_soil
user_accuracy_veg_strata <- user_accuracy_veg
producer_accuracy_soil_strata <- producer_accuracy_soil
producer_accuracy_veg_strata <- producer_accuracy_veg
str(validation)
str(UA)
# Schleife über alle Sets und ZEitpunkte mit stratifizierter aufteilung der Trainings-und Validierungsdaten
for (date in dates) {
  set_list_sub <- lapply(set_list_all, function(df) {
    df[df$date == date, ]
  })
  overall_accuracy_dates <- data.frame(set = character(), value = numeric(), variables = character(), stringsAsFactors = FALSE)
  user_accuracy_soil_dates <- data.frame(set = character(), value = numeric())
  user_accuracy_veg_dates <- data.frame(set = character(), value = numeric())
  producer_accuracy_soil_dates <- data.frame(set = character(), value = numeric())
  producer_accuracy_veg_dates <- data.frame(set = character(), value = numeric())

  for (set_name in names(set_list_all)) {
    set <- set_list_sub[[set_name]]
    # Nur die Spalten auswählen, die für die Stratifikation genutzt werden sollen
    strata_data <- set[, c("class", "BODTYP_K", "date")]

    # multi_strata erzeugt die Schichten (Strata)
    strata_factor <- multi_strata(strata_data, strategy = "interaction")

    # Daten partitionieren mit der stratification
    set.seed(161)
    partitions <- partition(strata_factor, p = c(train = 0.8, valid = 0.2))
    train_data <- set[partitions$train, -which(names(set) %in% c("date", "BODTYP_K"))]
    valid_data <- set[partitions$valid, ]
    train_data$class <- as.factor(train_data$class)
    set.seed(161)
    rf_model <- randomForest(class ~ ., data = train_data, ntree = 500)
    prediction <- predict(rf_model, newdata = valid_data)
    validation <- data.frame(prediction = as.numeric(prediction), actual = as.numeric(valid_data$class))
    OA <- sum(validation$prediction == validation$actual) / nrow(validation)
    #OOB <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
    UA <- diag(table(validation)) / rowSums(table(validation))
    PA <- diag(table(validation)) / colSums(table(validation))

    overall_accuracy_dates <- rbind(overall_accuracy_dates, data.frame(set = set_name, value = OA, variables = paste(names(set)[-(1:3)], collapse = ", ")))
    user_accuracy_soil_dates <- rbind(user_accuracy_soil_dates, data.frame(set = set_name, value = UA[1]))
    user_accuracy_veg_dates <- rbind(user_accuracy_veg_dates, data.frame(set = set_name, value = UA[2]))
    producer_accuracy_soil_dates <- rbind(producer_accuracy_soil_dates, data.frame(set = set_name, value = PA[1]))
    producer_accuracy_veg_dates <- rbind(producer_accuracy_veg_dates, data.frame(set = set_name, value = PA[2]))
  }
  names(overall_accuracy_dates)[names(overall_accuracy_dates) == "value"] <- paste(date)
  names(user_accuracy_soil_dates)[names(user_accuracy_soil_dates) == "value"] <- paste(date)
  names(user_accuracy_veg_dates)[names(user_accuracy_veg_dates) == "value"] <- paste(date)
  names(producer_accuracy_soil_dates)[names(producer_accuracy_soil_dates) == "value"] <- paste(date)
  names(producer_accuracy_veg_dates)[names(producer_accuracy_veg_dates) == "value"] <- paste(date)

  if (date != dates[length(dates)]) {
    overall_accuracy_strata <- base::merge(overall_accuracy_strata, overall_accuracy_dates[1:2], by = "set")
    user_accuracy_soil_strata <- base::merge(user_accuracy_soil_strata, user_accuracy_soil_dates, by = "set")
    user_accuracy_veg_strata <- base::merge(user_accuracy_veg_strata, user_accuracy_veg_dates, by = "set")
    producer_accuracy_soil_strata <- base::merge(producer_accuracy_soil_strata, producer_accuracy_soil_dates, by = "set")
    producer_accuracy_veg_strata <- base::merge(producer_accuracy_veg_strata, producer_accuracy_veg_dates, by = "set")
  } else {
    overall_accuracy_strata <- base::merge(overall_accuracy_strata, overall_accuracy_dates[1:3], by = "set")
    user_accuracy_soil_strata <- base::merge(user_accuracy_soil_strata, user_accuracy_soil_dates, by = "set")
    user_accuracy_veg_strata <- base::merge(user_accuracy_veg_strata, user_accuracy_veg_dates, by = "set")
    producer_accuracy_soil_strata <- base::merge(producer_accuracy_soil_strata, producer_accuracy_soil_dates, by = "set")
    producer_accuracy_veg_strata <- base::merge(producer_accuracy_veg_strata, producer_accuracy_veg_dates, by = "set")
  }
}
overall_accuracy_strata <- arrange(overall_accuracy_strata, desc(all_timepoints))
View(overall_accuracy_strata)
write.csv(overall_accuracy_strata, "Data/tables/random_forest_overall_accuracy_best_sets_strata_2class.csv", row.names = FALSE)
write.xlsx(
  list(
    overall_accuracy = overall_accuracy_strata,
    user_accuracy_soil = user_accuracy_soil_strata,
    user_accuracy_veg = user_accuracy_veg_strata,
    producer_accuracy_soil = producer_accuracy_soil_strata,
    producer_accuracy_veg = producer_accuracy_veg_strata
  ),
  file = "Data/tables/random_forest_accuracies_best_sets_strata_2class.xlsx",
  overwrite = TRUE
)

overall_accuracy_perdate <- overall_accuracy
user_accuracy_soil_perdate <- user_accuracy_soil
user_accuracy_veg_perdate <- user_accuracy_veg
producer_accuracy_soil_perdate <- producer_accuracy_soil
producer_accuracy_veg_perdate <- producer_accuracy_veg
# Schleife über alle Sets und Daten mit angepasster aufteilung der Trainings-und Validierungsdaten
for (date in dates) {
  set_list_sub <- lapply(set_list_all, function(df) {
    df[df$date == date, ]
  })
  overall_accuracy_dates <- data.frame(set = character(), value = numeric(), variables = character(), stringsAsFactors = FALSE)
  user_accuracy_soil_dates <- data.frame(set = character(), value = numeric())
  user_accuracy_veg_dates <- data.frame(set = character(), value = numeric())
  producer_accuracy_soil_dates <- data.frame(set = character(), value = numeric())
  producer_accuracy_veg_dates <- data.frame(set = character(), value = numeric())

  for (set_name in names(set_list_sub)) {
    valid_data <- set_list_sub[[set_name]]
    train_data <- set_list_all[[set_name]][set_list_all[[set_name]]$date != date, ]
    train_data <- train_data[, -which(names(train_data) %in% c("date"))]
    train_data$class <- as.factor(train_data$class)
    set.seed(161)
    rf_model <- randomForest(class ~ ., data = train_data, ntree = 500)
    prediction <- predict(rf_model, newdata = valid_data)
    validation <- data.frame(prediction = as.numeric(prediction), actual = as.numeric(valid_data$class))
    OA <- sum(validation$prediction == validation$actual) / nrow(validation)
    #OOB <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
    UA <- diag(table(validation)) / rowSums(table(validation))
    PA <- diag(table(validation)) / colSums(table(validation))

    overall_accuracy_dates <- rbind(overall_accuracy_dates, data.frame(set = set_name, value = OA, variables = paste(names(valid_data)[-(1:3)], collapse = ", ")))
    user_accuracy_soil_dates <- rbind(user_accuracy_soil_dates, data.frame(set = set_name, value = UA[1]))
    user_accuracy_veg_dates <- rbind(user_accuracy_veg_dates, data.frame(set = set_name, value = UA[2]))
    producer_accuracy_soil_dates <- rbind(producer_accuracy_soil_dates, data.frame(set = set_name, value = PA[1]))
    producer_accuracy_veg_dates <- rbind(producer_accuracy_veg_dates, data.frame(set = set_name, value = PA[2]))
  }
  names(overall_accuracy_dates)[names(overall_accuracy_dates) == "value"] <- paste(date)
  names(user_accuracy_soil_dates)[names(user_accuracy_soil_dates) == "value"] <- paste(date)
  names(user_accuracy_veg_dates)[names(user_accuracy_veg_dates) == "value"] <- paste(date)
  names(producer_accuracy_soil_dates)[names(producer_accuracy_soil_dates) == "value"] <- paste(date)
  names(producer_accuracy_veg_dates)[names(producer_accuracy_veg_dates) == "value"] <- paste(date)

  if (date != dates[length(dates)]) {
    overall_accuracy_perdate <- base::merge(overall_accuracy_perdate, overall_accuracy_dates[1:2], by = "set")
    user_accuracy_soil_perdate <- base::merge(user_accuracy_soil_perdate, user_accuracy_soil_dates, by = "set")
    user_accuracy_veg_perdate <- base::merge(user_accuracy_veg_perdate, user_accuracy_veg_dates, by = "set")
    producer_accuracy_soil_perdate <- base::merge(producer_accuracy_soil_perdate, producer_accuracy_soil_dates, by = "set")
    producer_accuracy_veg_perdate <- base::merge(producer_accuracy_veg_perdate, producer_accuracy_veg_dates, by = "set")
  } else {
    overall_accuracy_perdate <- base::merge(overall_accuracy_perdate, overall_accuracy_dates[1:3], by = "set")
    user_accuracy_soil_perdate <- base::merge(user_accuracy_soil_perdate, user_accuracy_soil_dates, by = "set")
    user_accuracy_veg_perdate <- base::merge(user_accuracy_veg_perdate, user_accuracy_veg_dates, by = "set")
    producer_accuracy_soil_perdate <- base::merge(producer_accuracy_soil_perdate, producer_accuracy_soil_dates, by = "set")
    producer_accuracy_veg_perdate <- base::merge(producer_accuracy_veg_perdate, producer_accuracy_veg_dates, by = "set")
  }
}
overall_accuracy_perdate <- arrange(overall_accuracy_perdate, desc(all_timepoints))
View(overall_accuracy_perdate)
write.csv(overall_accuracy_perdate, "Data/tables/random_forest_overall_accuracy_all_sets_val-per-date_2class.csv", row.names = FALSE)
write.xlsx(
  list(
    overall_accuracy = overall_accuracy_perdate,
    user_accuracy_soil = user_accuracy_soil_perdate,
    user_accuracy_veg = user_accuracy_veg_perdate,
    producer_accuracy_soil = producer_accuracy_soil_perdate,
    producer_accuracy_veg = producer_accuracy_veg_perdate
  ),
  file = "Data/tables/random_forest_accuracies_all_sets_val-per-date_2class.xlsx",
  overwrite = TRUE
)