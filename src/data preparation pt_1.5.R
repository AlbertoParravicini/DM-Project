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
dataset <- read.csv("Modified data/dataset_polimi.csv", stringsAsFactors=FALSE)


# Extract number from string
dataset$N_Zona <- strtoi(gsub("Zona_","",dataset$Zona))
dataset$N_Area <- strtoi(gsub("Area_","",dataset$Area))
dataset$N_Sottoarea <- strtoi(gsub("Sottoarea_","",dataset$Sottoarea))
dataset$N_Prodotto <- strtoi(gsub("Prodotto_","",dataset$Categoria_prodotto))


# Reorder table by key
dataset <- dataset[order(dataset$N_Zona, dataset$N_Area, dataset$N_Sottoarea, dataset$N_Prodotto, dataset$Data) , ]


# Reorder table by Data
#dataset <- dataset[order(dataset$Data) , ]


# Convert dates to class "Data"
dataset$Data <- as.Date(dataset$Data, format="%Y-%m-%d")


# Extract information from Data field
dataset$Giorno_Mese <- day(dataset$Data)
dataset$Giorno_Settimana <- ifelse(wday(dataset$Data) > 1, wday(dataset$Data) - 1, 7)
dataset$Giorno_Anno <- yday(dataset$Data)
dataset$Mese <- month(dataset$Data)
dataset$Settimana_Anno <- week(dataset$Data)
dataset$Anno <- year(dataset$Data)
dataset$Weekend <- ifelse(dataset$Giorno_Settimana > 5, 1, 0)


# Extract seasons from Data field
dataset$Stagione <- ifelse(dataset$Mese <= 2 | dataset$Mese >= 12, 'inverno',
                                ifelse(3 <= dataset$Mese & dataset$Mese <= 5, 'primavera',
                                       ifelse(6 <= dataset$Mese & dataset$Mese <= 8, 'estate',
                                              ifelse(9 <= dataset$Mese & dataset$Mese <= 11, 'autunno','?'))))

# Assegna una chiave in base a zona, area e sottoarea
dataset$Key <- paste(dataset$Zona, paste(dataset$Area, dataset$Sottoarea, sep="-"), sep="-")


# Weekend average vs other days average
mean(filter(dataset, Weekend == 1)$Vendite)
mean(filter(dataset, Weekend == 0)$Vendite)
