packages <- c("randomForest", "splitTools")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)  # Paket installieren
    library(pkg, character.only = TRUE)  # Paket laden
  }
}

# load data
data <- read.csv("Data/tables/extracted_values.csv")
head(data)
# count Nas
colSums(is.na(data))
data <- na.omit(data)

# Nur die Spalten auswählen, die für die Stratifikation genutzt werden sollen
strata_data <- data[, c("class", "BODTYP_K", "date")]

# multi_strata erzeugt die Schichten (Strata)
strata_factor <- multi_strata(strata_data, strategy = "interaction")

# Daten partitionieren mit der stratification
set.seed(161)
partitions <- partition(strata_factor, p = c(train = 0.9, valid = 0.1))

train_data <- data[partitions$train, c("class", "BODTYP_K", "RVI", "BSI", "NBR2") ]
valid_data <- data[partitions$valid, ]
train_data$class <- as.factor(train_data$class)
head(train_data)
str(train_data)
rf_model <- randomForest(class ~ ., data = train_data, ntree = 500)
prediction <- predict(rf_model, newdata = valid_data)
importance <- importance(rf_model)
