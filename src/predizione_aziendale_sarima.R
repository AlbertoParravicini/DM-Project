library(xts)
library(ggplot2)
library(ggthemes) # visualization
library(dplyr)
library(tseries)
library(PerformanceAnalytics)
library(forecast)
library(astsa)
library(Metrics)
library(normwhn.test)
library(nortest)
library(nortestARMA)


# Import data
dataset <- read.csv("Modified data/vendite_giornaliere_prod.csv", stringsAsFactors=FALSE)

colnames(dataset)[which(colnames(dataset) == 'vendite_giorn_prod')] <- 'vendite'

dataset$zona <- 999

n_test_rows <- 10
current_prod <- 1

dataset <- filter(dataset, prod == current_prod)

tot_rows <- nrow(dataset)
n_train_rows <- tot_rows - n_test_rows

data_train <- dataset[1:n_train_rows,]
data_test <- dataset[(n_train_rows+1):tot_rows,]

sarima_prediction(data_train = data_train, data_test = data_test, num_prod = current_prod, num_zona = 999, details = T)

sa <- sarima_prediction(dataset, prediction_length = 10, num_prod = 1, num_zona = 999, details = T)

sa@prediction
