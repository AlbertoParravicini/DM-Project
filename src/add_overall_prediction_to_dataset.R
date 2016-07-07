library(xts)
library(ggplot2)
library(dplyr)
library(tseries)
library(PerformanceAnalytics)
library(forecast)
library(astsa)
library(Metrics)
library(ggthemes) # visualization

prediction_length = 10



# DATA IMPORT AND CLEANING
# ------------------------------------------------------
# ------------------------------------------------------


dataset <- read.csv("Modified data/dataset_polimi_clusterized.csv", stringsAsFactors=FALSE, row.names=NULL)
# Remove the x column, if present
dataset <- dataset[ , !(names(dataset) %in% c("X"))]


# Convert dates to class "Data"
dataset$data <- as.Date(dataset$data)
# Convert "vendite" to numeric values if needed
if (class(dataset$vendite) == "factor") {
  dataset$vendite <- as.numeric(levels(dataset$vendite))[dataset$vendite]
}

# Turn some features to factors
factorVars <- c('zona','area', "sottoarea",
                'prod','giorno_mese', "giorno_settimana", "giorno_anno", "mese", "settimana_anno", "anno", "weekend","stagione", "key", "azienda_chiusa", "primo_del_mese", "cluster3", "cluster6", "cluster20")

dataset[factorVars] <- lapply(dataset[factorVars], function(x) as.factor(x))

summary(dataset)

# Use the exogen signal of the overall sales
vendite_giornaliere_prod <- read.csv("Modified data/vendite_giornaliere_prod.csv", row.names=NULL, stringsAsFactors=FALSE)
vendite_giornaliere_prod$prod <- as.factor(vendite_giornaliere_prod$prod)

# Turn dates to "Date" class
dataset$data <- as.Date(as.character(dataset$data),format="%Y-%m-%d")
vendite_giornaliere_prod$data <- as.Date(as.character(vendite_giornaliere_prod$data),format="%Y-%m-%d")

total_table <- merge(dataset, vendite_giornaliere_prod, by = c("prod", "data"), all.x = T)
# Rename "vendite_giorn_prod.y" to "vendite_giorn_prod"
names(total_table)[names(total_table) == 'vendite_giorn_prod.y'] <- 'vendite_giorn_prod'
View(total_table)

write.csv(total_table, file="Modified data/dataset_polimi_clusterized_tot_pred.csv", row.names = FALSE)

