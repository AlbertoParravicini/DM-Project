library(xts)
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)

setwd("~/DM-Project")
dataset <- read.csv("~/DM-Project/Modified data/dataset_polimi.csv", stringsAsFactors=FALSE, row.names=NULL)

# Remove the x column, if present
dataset <- dataset[ , !(names(dataset) %in% c("X"))]

# Build a smaller datasetset, for testing
# dataset <- dataset[sample(1:nrow(dataset), 1000), ]

# Convert dates to class "Data"
dataset$Data <- as.Date(dataset$Data)

# Remove unneded portions of strings
dataset <- as.data.frame(sapply(dataset, function(x) gsub("Zona_", "", x)))
dataset <- as.data.frame(sapply(dataset, function(x) gsub("Area_", "", x)))
dataset <- as.data.frame(sapply(dataset, function(x) gsub("Sottoarea_", "", x)))
dataset <- as.data.frame(sapply(dataset, function(x) gsub("Prodotto_", "", x)))

# Rename "Categoria_prodotto" to something shorter
names(dataset)[names(dataset) == 'Categoria_prodotto'] <- 'prod'

# Put column names to lower case
names(dataset) <- tolower(names(dataset))

# Remove other unnedeed columns, if present
dataset <- dataset[ , !(names(dataset) %in% c("n_zona", "n_area", "n_sottoarea", "n_prodotto"))]

# Rename "stagione_mese" to "stagione"
names(dataset)[names(dataset) == 'stagione_mese'] <- 'stagione'

# Rename other columns to something shorter
names(dataset)[names(dataset) == 'giornidellasettimana'] <- 'giorno_sett'
names(dataset)[names(dataset) == 'giornodelmese'] <- 'giorno_mese'
names(dataset)[names(dataset) == 'giornodellanno'] <- 'giorno_anno'

dataset$data <- as.Date(dataset$data, format="%Y-%m-%d")

if (class(dataset$vendite) == "factor") {
  dataset$vendite <- as.numeric(levels(dataset$vendite))[dataset$vendite]
}

summary(dataset)
class(dataset$data)

write.csv(dataset, file="Modified data/dataset_polimi.csv", row.names = F)