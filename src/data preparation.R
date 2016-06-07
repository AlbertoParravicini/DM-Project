library(lubridate)
library(zoo)
library(dplyr)
library(xts)
library(ggplot2)
library(dplyr)

setwd("~/DM-Project")
dataset_polimi <- read.csv("~/DM-Project/Original data/dataset_polimi.csv", stringsAsFactors=FALSE)

# Estrai il giorno nel mese
dataset_polimi$Giorno_Mese <- day(dataset_polimi$Data)
# Estrai il giorno nella settimana
dataset_polimi$Giorno_Settimana <- wday(dataset_polimi$Data)
# Estrai il giorno nell'anno
dataset_polimi$Giorno_Anno <- yday(dataset_polimi$Data)
# Estrai il mese
dataset_polimi$Mese <- month(dataset_polimi$Data)
# Estrai la settimana dell'anno
dataset_polimi$Settimana_Anno <- week(dataset_polimi$Data)
# Estrai l'anno
dataset_polimi$Anno <- year(dataset_polimi$Data)


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
