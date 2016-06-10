library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # dataset_polimi manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm
library(ranger) # faster version of randomForest
library(nnet) # For binarization
library(zoo) # For time series

# Import data

dataset_polimi <- read.csv("Original data/dataset_polimi.csv", stringsAsFactors=FALSE)


# Extract number from string
dataset_polimi$N_Zona <- strtoi(gsub("Zona_","",dataset_polimi$Zona))
dataset_polimi$N_Area <- strtoi(gsub("Area_","",dataset_polimi$Area))
dataset_polimi$N_Sottoarea <- strtoi(gsub("Sottoarea_","",dataset_polimi$Sottoarea))
dataset_polimi$N_Prodotto <- strtoi(gsub("Prodotto_","",dataset_polimi$Categoria_prodotto))


# Reorder table by key
dataset_polimi <- dataset_polimi[order(dataset_polimi$N_Zona, dataset_polimi$N_Area, dataset_polimi$N_Sottoarea, dataset_polimi$N_Prodotto, dataset_polimi$Data) , ]


# Extract information from Data field
dataset_polimi$Giorno_Mese <- day(dataset_polimi$Data)
dataset_polimi$Giorno_Settimana <- ifelse(wday(dataset_polimi$Data) > 1, wday(dataset_polimi$Data) - 1, 7)
dataset_polimi$Giorno_Anno <- yday(dataset_polimi$Data)
dataset_polimi$Mese <- month(dataset_polimi$Data)
dataset_polimi$Settimana_Anno <- week(dataset_polimi$Data)
dataset_polimi$Anno <- year(dataset_polimi$Data)
dataset_polimi$Weekend <- ifelse(dataset_polimi$Giorno_Settimana > 5, 1, 0)


# Extract seasons from Data field
dataset_polimi$Stagione_Mese <- ifelse(dataset_polimi$Mese <= 2 | dataset_polimi$Mese >= 12, 'inverno',
                                ifelse(3 <= dataset_polimi$Mese & dataset_polimi$Mese <= 5, 'primavera',
                                       ifelse(6 <= dataset_polimi$Mese & dataset_polimi$Mese <= 8, 'estate',
                                              ifelse(9 <= dataset_polimi$Mese & dataset_polimi$Mese <= 11, 'autunno','?'))))

# Assegna una chiave in base a zona, area e sottoarea
dataset_polimi$Key <- paste(dataset_polimi$Zona, paste(dataset_polimi$Area, dataset_polimi$Sottoarea, sep="-"), sep="-")




sample_set <- dataset_polimi[sample(1:nrow(dataset_polimi), 1000), ]
write.csv(sample_set, file="Modified data/sample_set_polimi.csv")
write.csv(dataset_polimi, file="Modified data/dataset_polimi.csv")
