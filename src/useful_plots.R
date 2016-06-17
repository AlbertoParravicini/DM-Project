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

# Convert dates to class "data"
dataset$data <- as.Date(dataset$data, format="%Y-%m-%d")
dataset$prod <- paste ("p",dataset$prod, sep = "_", collapse = NULL)


# Plot Media vendite per Giorno e Prodotto
giorno_settimana_dataset <- dataset %>%
  group_by(giorno_settimana, prod) %>%
  summarise(vendite = mean(vendite))

ggplot(giorno_settimana_dataset, aes(x = giorno_settimana, y = vendite, fill = prod)) +
  geom_bar(stat = 'identity', position = 'stack', colour = 'black') +
  labs(y = 'Media vendite', 
       x = 'Giorno',
       title = 'Media vendite per Giorno e Prodotto') +
  theme(axis.text=element_text(color = "black")) +
  scale_x_discrete(limits = c("lun","mar","mer","gio","ven","sab","dom")) + 
  scale_y_continuous(breaks=seq(0, 10, 0.25))



# Plot Media vendite per Mese e Prodotto
Mese_dataset <- dataset %>%
  group_by(mese, prod) %>%
  summarise(vendite = mean(vendite))

ggplot(Mese_dataset, aes(x = mese, y = vendite, fill = prod)) +
  geom_bar(stat = 'identity', position = 'stack', colour = 'black') +
  labs(y = 'Media vendite', 
       x = 'Mese',
       title = 'Media vendite per Mese e Prodotto') +
  theme(axis.text=element_text(color = "black")) +
  scale_x_discrete(limits = Mese_dataset$mese) + 
  scale_y_continuous(breaks=seq(0, 10, 0.25))



# Plot Media vendite per Stagione e Prodotto
Stagione_dataset <- dataset %>%
  group_by(stagione, prod) %>%
  summarise(vendite = mean(vendite))

ggplot(Stagione_dataset, aes(x = stagione, y = vendite, fill = prod)) +
  geom_bar(stat = 'identity', position = 'stack', colour = 'black') +
  labs(y = 'Media vendite', 
       x = 'Stagione',
       title = 'Media vendite per Stagione e Prodotto') +
  theme(axis.text=element_text(color = "black")) +
  scale_x_discrete(limits = Mese_dataset$stagione) + 
  scale_y_continuous(breaks=seq(0, 10, 0.25))



# Plot Media vendite per zona e Prodotto
zona_dataset <- dataset %>%
  group_by(zona, prod) %>%
  summarise(vendite = mean(vendite))

ggplot(zona_dataset, aes(x = zona, y = vendite, fill = prod)) +
  geom_bar(stat = 'identity', position = 'stack', colour = 'black') +
  labs(y = 'Media vendite', 
       x = 'zona',
       title = 'Media vendite per zona e Prodotto') +
  theme(axis.text=element_text(size = 5, color = "black")) +
  scale_x_discrete(limits = Mese_dataset$zona) + 
  scale_y_continuous(breaks=seq(0, 10, 0.25))