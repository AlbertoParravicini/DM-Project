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
predset <- read.csv("Modified data/predset.csv", stringsAsFactors=FALSE)

predset$data <- as.Date(as.character(predset$data),format="%Y-%m-%d")

# Extract information from data field
predset$Giorno_Mese <- day(predset$data)
predset$Giorno_Settimana <- ifelse(wday(predset$data) > 1, wday(predset$data) - 1, 7)
predset$Giorno_Anno <- yday(predset$data)
predset$Mese <- month(predset$data)
predset$Settimana_Anno <- week(predset$data)
predset$Anno <- year(predset$data)
predset$Weekend <- ifelse(predset$Giorno_Settimana > 5, 1, 0)


# Extract seasons from data field
predset$Stagione_Mese <- ifelse(predset$Mese <= 2 | predset$Mese >= 12, 'inverno',
                                       ifelse(3 <= predset$Mese & predset$Mese <= 5, 'primavera',
                                              ifelse(6 <= predset$Mese & predset$Mese <= 8, 'estate',
                                                     ifelse(9 <= predset$Mese & predset$Mese <= 11, 'autunno','?'))))


# Put column names to lower case
names(predset) <- tolower(names(predset))

# Rename "stagione_mese" to "stagione"
names(predset)[names(predset) == 'stagione_mese'] <- 'stagione'

# Rename other columns to something shorter
names(predset)[names(predset) == 'giorno_settimana'] <- 'giorno_sett'

# Rename other columns to something shorter
names(predset)[names(predset) == 'giorno_settimana'] <- 'giorno_sett'

# Create primo_del_mese
predset$primo_del_mese <- ifelse(predset$giorno_mese == 1, 1, 0)


write.csv(predset, file="Modified data/predset_final.csv", row.names = F)