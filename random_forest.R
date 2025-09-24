packages <- c("randomForest", "splitTools")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}
## aus cohens_d (stats.R):
## best optical: NBR2, B8A, MSAVI8A, MSAVI2, BSI8A
## best SAR: RVI_buff, VH_mean, VH_variance
2^8
# load data
data <- read.csv("Data/tables/extracted_values.csv")
names(data)
# subset data by best variables
data <- data %>%
  dplyr::select(class, BODTYP_K, date, NBR2, B8A, MSAVI8A, MSAVI2, BSI8A, RVI_buff, VH_mean, VH_variance)
# count Nas
colSums(is.na(data))
data <- na.omit(data)
# split Dataset into different sets of timepoints and variable combinations
n_class <- 2 # 2 for 2-class, 3 for 3-class
if (n_class == 2) {
  data$class <- ifelse(data$class == 3, 2, data$class)
}
# für jede Variable einzeln testen
variables <- names(data)[-c(1,2,3)]
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
View(overall_accuracy_var)
write.csv(overall_accuracy_var, "Data/tables/random_forest_overall_accuracy_per_variable_2class.csv", row.names = FALSE)
# test for different variable combinations
dates <- unique(data$date)
# Split dataset into different sets of variable combinations
set_list_all <- list(
  all = data %>% dplyr::select(class, BODTYP_K, date, everything()),
  optical_indices = data %>% dplyr::select(class, BODTYP_K, date, NBR2, MSAVI2, MSAVI8A, BSI8A),
  optical_8A = data %>% dplyr::select(class, BODTYP_K, date, B8A, NBR2, MSAVI2),
  optical_x = data %>% dplyr::select(class, BODTYP_K, date, NBR2, MSAVI2, BSI8A),
  #all_bands = data %>% dplyr::select(class, BODTYP_K, date, starts_with("B")),
  all_indices = data %>% dplyr::select(class, BODTYP_K, date, NBR2, MSAVI2, MSAVI8A, RVI_buff, VH_variance),
  all_SAR = data %>% dplyr::select(class, BODTYP_K, date, VH_mean, VH_variance, RVI_buff),
  SAR_buff = data %>% dplyr::select(class, BODTYP_K, date, VH_variance, RVI_buff),
  SAR_mean = data %>% dplyr::select(class, BODTYP_K, date, VH_mean, VH_variance)
  #all_SAR_texture = data %>% dplyr::select(class, BODTYP_K, date, VH_variance, VH_mean, VH_dissimilarity),
  #all_SAR_RVI = data %>% dplyr::select(class, BODTYP_K, date, RVI),
  #best_SAR = data %>% dplyr::select(class, BODTYP_K, date, RVI_buff, VH_variance, VH_buff, VH_dissimilarity),
  #optical_plus_RVI = data %>% dplyr::select(class, BODTYP_K, date, BSI, NBR2, RVI, MSAVI2, RVI, VH_mean, VH_variance)
)
# Schleife über alle sets mit und ohne Datum als Variable
overall_accuracy_all <- data.frame(set = names(set_list_all))
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
  for (set_name in names(set_list_all)) {
    start_time <- Sys.time()
    set <- set_list_all[[set_name]]
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

    overall_accuracy <- rbind(overall_accuracy, data.frame(set = set_name, all_timepoints = OA))
    end_time <- Sys.time()
    print(end_time - start_time)
  }
  if (option == "FALSE") {
    names(overall_accuracy)[names(overall_accuracy) == "all_timepoints"] <- paste("OA_no_date")
  } else if (option == "TRUE") {
    names(overall_accuracy)[names(overall_accuracy) == "all_timepoints"] <- paste("OA_with_date")
  }
  overall_accuracy_all <- merge(overall_accuracy_all, overall_accuracy, by = "set")
}
overall_accuracy_all <- arrange(overall_accuracy_all, desc(OA_with_date))
View(overall_accuracy_all)
write.csv(overall_accuracy_all, "Data/tables/random_forest_overall_accuracy_all_sets_2class.csv", row.names = FALSE)
overall_accuracy_strata <- overall_accuracy_all
# Schleife über alle Sets und ZEitpunkte mit stratifizierter aufteilung der Trainings-und Validierungsdaten
for (date in dates) {
  set_list_sub <- lapply(set_list_all, function(df) {
    df[df$date == date, ]
  })
  overall_accuracy_dates <- data.frame(
    set = character(),
    value = numeric(),
    #  oob_error = numeric(),
    variables = character(),
    stringsAsFactors = FALSE
  )
  for (set_name in names(set_list_sub)) {
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
    OOB <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]
    overall_accuracy_dates <- rbind(overall_accuracy_dates, data.frame(set = set_name, value = OA, variables = paste(names(set)[-(1:3)], collapse = ", ")))
  }
  names(overall_accuracy_dates)[names(overall_accuracy_dates) == "value"] <- paste(date)
  if (date != dates[length(dates)]) {
    overall_accuracy_strata <- base::merge(overall_accuracy_strata, overall_accuracy_dates[1:2], by = "set")
  } else {
    overall_accuracy_strata <- base::merge(overall_accuracy_strata, overall_accuracy_dates[1:3], by = "set")
  }
}
overall_accuracy_strata <- arrange(overall_accuracy_strata, desc(OA_with_date))
View(overall_accuracy_strata)
write.csv(overall_accuracy_strata, "Data/tables/random_forest_overall_accuracy_all_sets_strata_2class.csv", row.names = FALSE)

# Schleife über alle Sets und Daten mit angepasster aufteilung der Trainings-und Validierungsdaten
for (date in dates) {
  set_list_sub <- lapply(set_list_all, function(df) {
    df[df$date == date, ]
  })
  overall_accuracy_dates <- data.frame(
    set = character(),
    value = numeric(),
    variables = character(),
    stringsAsFactors = FALSE
  )
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
    overall_accuracy_dates <- rbind(overall_accuracy_dates, data.frame(set = set_name, value = OA, variables = paste(names(valid_data)[-(1:3)], collapse = ", ")))
  }
  names(overall_accuracy_dates)[names(overall_accuracy_dates) == "value"] <- paste(date)
  if (date != dates[length(dates)]) {
    overall_accuracy_all <- base::merge(overall_accuracy_all, overall_accuracy_dates[1:2], by = "set")
  } else {
    overall_accuracy_all <- base::merge(overall_accuracy_all, overall_accuracy_dates[1:3], by = "set")
  }
}
overall_accuracy_all <- arrange(overall_accuracy_all, desc(OA_with_date))
View(overall_accuracy_all)
write.csv(overall_accuracy_all, "Data/tables/random_forest_overall_accuracy_all_sets_val-per-date_2class.csv", row.names = FALSE)

stop("Stop here")
str(set_list_sub[[set_name]])
unique(set_list_sub[[set_name]]$date)
unique(train_data$date)
unique(valid_data$date)
str(set_list_sub)
str(valid_data)
str(train_data)
head(valid_data)
head(train_data)
str(set_list_all)
names(train_data)
stop("Stop here")
importance <- rf_model$importance
# Nur die Spalten auswählen, die für die Stratifikation genutzt werden sollen
strata_data <- data[, c("class", "BODTYP_K", "date")]

# multi_strata erzeugt die Schichten (Strata)
strata_factor <- multi_strata(strata_data, strategy = "interaction")

# Daten partitionieren mit der stratification
set.seed(161)
partitions <- partition(strata_factor, p = c(train = 0.8, valid = 0.2))

train_data <- data[partitions$train, c("class", "BODTYP_K", "RVI", "BSI", "NBR2")]
valid_data <- data[partitions$valid, ]
train_data$class <- as.factor(train_data$class)
head(train_data)
str(train_data)
rf_model <- randomForest(class ~ ., data = train_data, ntree = 500)
prediction <- predict(rf_model, newdata = valid_data)

x <- data.frame(set = set_name, value = OA, variables = paste(names(set)[-(1:3)], collapse = ", "))
View(x)
names(x)
names(overall_accuracy_dates)
names(overall_accuracy_all)
unique(set_list_all$all$date)
exists(".Random.seed")
# confusion matrix
confusion_matrix <- table(Predicted = prediction, Actual = valid_data$class)

predict_2class <- ifelse(prediction == 3, 2, prediction)
valid_2class <- ifelse(valid_data$class == 3, 2, valid_data$class)
confusion_matrix_2class <- table(Predicted = predict_2class, Actual = valid_2class)
OA_2class <- sum(diag(confusion_matrix_2class)) / sum(confusion_matrix_2class)
OA_2class
#View(read.csv(r"(Data/tables/cohens_d_overall.csv)"))

rf_model$importance
