library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm
library(ranger) # faster version of randomForest
library(nnet) # For binarization
library(zoo) # For time series

# Import data
dataset <- read.csv("Modified data/dataset_polimi_complete.csv", stringsAsFactors=FALSE)

# Determina date predizioni
n_date <- 10

dataset$data <- as.Date(as.character(dataset$data),format="%Y-%m-%d")
last_date <- max(unique(dataset$data))

for(i in seq(1:n_date))
  date_predizioni[i] <- last_date + i

# Inizializza il predset con attributi unici
predset <- select(dataset, prod, zona, area, sottoarea, key, latitudine, longitudine)
predset <- unique(predset)

# Join con le date
predset <- merge(date_predizioni, predset)

# Rinomina la colonna delle date
colnames(predset)[which(colnames(predset) == 'x')] <- 'data'

write.csv(predset, file="Modified data/predset.csv", row.names = F)