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
#setwd("C:/Users/Storna/Desktop/dm_proj/workspace")
#dataset_polimi <- read.csv("dataset_polimi.csv", stringsAsFactors=FALSE)


# Extract number from string
dataset_polimi$N_Zona <- strtoi(gsub("Zona_","",dataset_polimi$Zona))
dataset_polimi$N_Area <- strtoi(gsub("Area_","",dataset_polimi$Area))
dataset_polimi$N_Sottoarea <- strtoi(gsub("Sottoarea_","",dataset_polimi$Sottoarea))
dataset_polimi$N_Prodotto <- strtoi(gsub("Prodotto_","",dataset_polimi$Categoria_prodotto))


# Reorder table by key
dataset_polimi <- dataset_polimi[order(dataset_polimi$N_Zona, dataset_polimi$N_Area, dataset_polimi$N_Sottoarea, dataset_polimi$N_Prodotto, dataset_polimi$Data) , ]


# Extract information from Data field
dataset_polimi$Giorno_Mese <- day(dataset_polimi$Data)
dataset_polimi$Giorno_Settimana <- wday(dataset_polimi$Data)
dataset_polimi$Giorno_Anno <- yday(dataset_polimi$Data)
dataset_polimi$Mese <- month(dataset_polimi$Data)
dataset_polimi$Settimana_Anno <- week(dataset_polimi$Data)
dataset_polimi$Anno <- year(dataset_polimi$Data)


# Extract seasons from Data field
dataset_polimi$Stagione_Mese <- ifelse(dataset_polimi$Mese <= 2 | dataset_polimi$Mese >= 12, 'inverno',
                                ifelse(3 <= dataset_polimi$Mese & dataset_polimi$Mese <= 5, 'primavera',
                                       ifelse(6 <= dataset_polimi$Mese & dataset_polimi$Mese <= 8, 'estate',
                                              ifelse(9 <= dataset_polimi$Mese & dataset_polimi$Mese <= 11, 'autunno',''))))

# Assegna una chiave in base a zona, area e sottoarea
dataset_polimi$Key <- paste(dataset_polimi$Zona, paste(dataset_polimi$Area, dataset_polimi$Sottoarea, sep="-"), sep="-")


# Nota: la settiman è calcolata a partire dal primo gennaio, senza guardare che giorno sia (e.g. settimana inizia dal giovedi per il 2014)


# Somma parziale nella settimana
dataset_polimi$sp_sett <- 0
for (i in 1:nrow(dataset_polimi)){
  myProdotto <- dataset_polimi[i,]$Categoria_prodotto
  myKey <- dataset_polimi[i,]$Key
  myGiorno_Anno <- dataset_polimi[i,]$Giorno_Anno
  mySettimana <- dataset_polimi[i,]$Settimana_Anno
  myAnno <- dataset_polimi[i,]$Anno
  temp <- filter(dataset_polimi, Categoria_prodotto==myProdotto, Key==myKey, Settimana_Anno==mySettimana, Anno==myAnno, Giorno_Anno<=myGiorno_Anno)
  dataset_polimi[i,]$sp_sett <- sum(as.numeric(temp$Vendite))
}


# Somma parziale nel mese
dataset_polimi$sp_mese<-0
for (i in 1:nrow(dataset_polimi)){
  myProdotto <- dataset_polimi[i,]$Categoria_prodotto
  myKey <- dataset_polimi[i,]$Key
  myGiorno_Anno <- dataset_polimi[i,]$Giorno_Anno
  myMese <- dataset_polimi[i,]$Mese
  myAnno <- dataset_polimi[i,]$Anno
  temp <- filter(dataset_polimi, Categoria_prodotto==myProdotto, Key==myKey, Mese==myMese, Anno==myAnno, Giorno_Anno<=myGiorno_Anno)
  dataset_polimi[i,]$sp_mese <- sum(as.numeric(temp$Vendite))
}

# Somma parziale nell'anno
dataset_polimi$sp_anno<-0
for (i in 1:nrow(dataset_polimi)){
  myProdotto <- dataset_polimi[i,]$Categoria_prodotto
  myKey <- dataset_polimi[i,]$Key
  myGiorno_Anno <- dataset_polimi[i,]$Giorno_Anno
  myAnno <- dataset_polimi[i,]$Anno
  temp <- filter(dataset_polimi, Categoria_prodotto==myProdotto, Key==myKey, Anno==myAnno, Giorno_Anno<=myGiorno_Anno)
  dataset_polimi[i,]$sp_anno <- sum(as.numeric(temp$Vendite))
}

sample_set <- dataset_polimi[sample(1:nrow(dataset_polimi), 1000), ]
write.csv(sample_set, file="Modified data/sample_set_polimi.csv")
write.csv(dataset_polimi, file="Modified data/dataset_polimi.csv")
