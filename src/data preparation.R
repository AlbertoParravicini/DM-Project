library(lubridate)
library(xts)
library(ggplot2)
library(dplyr)

setwd("~/DM-Project")
dataset_polimi <- read.csv("~/DM-Project/Original data/dataset_polimi.csv", stringsAsFactors=FALSE)

# Estrai il giorno nel mese
dataset_polimi$GiornoDelMese <- day(dataset_polimi$Data)
# Estrai il giorno nella settimana
dataset_polimi$GiorniDellaSettimana <- wday(dataset_polimi$Data)
# Estrai il giorno nell'anno
dataset_polimi$GiornoDellAnno <- yday(dataset_polimi$Data)
# Estrai il mese
dataset_polimi$Mese <- month(dataset_polimi$Data)
# Estrai l'anno
dataset_polimi$Anno <- year(dataset_polimi$Data)


# Assegna una chiave in base a zona, area e sottoarea
dataset_polimi$Key <- paste(dataset_polimi$Zona, paste(dataset_polimi$Area, dataset_polimi$Sottoarea, sep="-"), sep="-")

sample_set <- dataset_polimi[sample(1:nrow(dataset_polimi), 1000), ]
write.csv(sample_set, file="Modified data/sample_set_polimi.csv")
write.csv(dataset_polimi, file="Modified data/dataset_polimi.csv")