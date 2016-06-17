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
dataset <- read.csv("Modified data/dataset_polimi_final.csv", stringsAsFactors=FALSE, row.names=NULL)

# N di sottoaree per prodotto
nrow(filter(dataset, data == "2014-01-01", prod == 1))

vendite_giornaliere <- select(dataset, prod, key, data, vendite)
vendite_giornaliere <- group_by(vendite_giornaliere, prod, data)
vendite_giornaliere <- summarise(vendite_giornaliere, vendite_giorn_prod = sum(vendite))

vendite_giornaliere_tot <- summarise(group_by(vendite_giornaliere,data), vendite_giorn_tot = sum(vendite_giorn_prod))
vendite_giornaliere_tot$azienda_chiusa <- ifelse(vendite_giornaliere_tot$vendite_giorn_tot > 0, 0, 1)

dataset <- merge(dataset, vendite_giornaliere, by = c("prod","data"))
dataset <- merge(dataset, vendite_giornaliere_tot, by = c("data"))

#PLOT
vendite_giornaliere_tot$data <- as.Date(vendite_giornaliere_tot$data, format="%Y-%m-%d")
# Andatura vendite azienda
ggplot(vendite_giornaliere_tot, aes(x = data, y = vendite_giorn_tot)) +
  geom_bar(stat = 'identity', position = 'dodge', colour = 'black') +
  labs(y = 'Vendite', 
       x = 'Giorno',
       title = 'Andatura vendite azienda') +
  theme(axis.text=element_text(color = "black"), axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(breaks=seq(0, 3000, 250)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")


write.csv(vendite_giornaliere, file="Modified data/vendite_giornaliere_prod.csv", row.names = F)
write.csv(vendite_giornaliere_tot, file="Modified data/vendite_giornaliere_tot.csv", row.names = F)
write.csv(dataset, file="Modified data/dataset_polimi_final_vendite_giorn.csv", row.names = F)
