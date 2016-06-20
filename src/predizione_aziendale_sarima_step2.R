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



# ===================== TEST ============================
# Import function
source("src/sarima.R")

# Import data
dataset <- read.csv("Modified data/vendite_giornaliere_prod.csv", stringsAsFactors=FALSE)

colnames(dataset)[which(colnames(dataset) == 'vendite_giorn_prod')] <- 'vendite'

dataset$zona <- 999
current_prod <- 1
n_test_rows <- 30


dataset <- filter(dataset, prod == current_prod)
dataset$data <- as.Date(as.character(dataset$data),format="%Y-%m-%d")

data_train <- filter(dataset, data <= max(data) - n_test_rows)
data_test <- filter(dataset, data > max(data) - n_test_rows)



sarima_prediction(data_train = data_train, data_test = data_test, num_prod = current_prod, num_zona = 999, details = T)

pred_test_regressors(min(data_test$data), nrow(data_test), num_prod = 1)



# ===================== PREDICTION PROD ============================
prediction_prod <- function(num_prod) {
  # Import function
  source("src/sarima.R")
  
  # Data preprocessing
  dataset <- read.csv("Modified data/vendite_giornaliere_prod.csv", stringsAsFactors=FALSE)
  colnames(dataset)[which(colnames(dataset) == 'vendite_giorn_prod')] <- 'vendite'
  dataset$zona <- 999
  dataset <- filter(dataset, prod == num_prod)
  
  # Prediction
  #sa <- sarima_prediction(dataset, prediction_length = 10, num_prod = num_prod, num_zona = 999, details = T)
  sa <- pred_test_regressors(max(dataset$data), 10, num_prod = num_prod)
  
  
  sa
  
  # Data postprocessing
  dataset_complete <- read.csv("Modified data/dataset_polimi_final_with_holidays_v2.csv", stringsAsFactors=FALSE)
  
  # Determina date predizioni
  n_date <- 10
  
  dataset_complete$data <- as.Date(as.character(dataset_complete$data),format="%Y-%m-%d")
  last_date <- max(unique(dataset_complete$data))
  
  date_predizioni <- c()
  predizioni <- c()
  
  for(i in seq(1:n_date)){
    date_predizioni[i] <- last_date + i
    predizioni[i] <- sa[i]
  }
  
  predset <- data.frame(date_predizioni, predizioni)
  predset$prod <- num_prod
  
  
  # Rinomina la colonna delle date
  colnames(predset)[which(colnames(predset) == 'date_predizioni')] <- 'data'
  colnames(predset)[which(colnames(predset) == 'predizioni')] <- 'vendite_giorn_prod'
  
  return(predset)
}

predset_1 <- prediction_prod(1)
predset_2 <- prediction_prod(2)
predset <- rbind(predset_1, predset_2)
predset
predset$data <- as.Date(predset$data)

write.csv(predset, file="Modified data/predizione_aziendale.csv", row.names = F)