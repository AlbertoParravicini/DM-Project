library(dplyr)
library(ranger)
library(ggplot2)
library(ggthemes)
library(zoo)
library(xgboost)

dataset_polimi <- read.csv("~/DM-Project/Modified data/dataset_polimi_complete.csv", stringsAsFactors=FALSE, row.names=NULL)

# dataset_polimi <- dataset_polimi_complete

giorni_test <- 10

data_train <- filter(dataset_polimi, anno < 2016 | giorno_anno <= (140-giorni_test))

data_test <- filter(dataset_polimi, anno==2016 & giorno_anno > (140-giorni_test))


# Convert dates to class "Data"
dataset_polimi$data <- as.Date(dataset_polimi$data)
data_train$data <- as.Date(data_train$data)
data_test$data <- as.Date(data_test$data)

# Convert "vendite" to numeric values if needed
if (class(dataset_polimi$vendite) == "factor") {
  dataset_polimi$vendite <- as.numeric(levels(dataset_polimi$vendite))[dataset_polimi$vendite]
}
if (class(data_train$vendite) == "factor") {
  data_train$vendite <- as.numeric(levels(data_train$vendite))[data_train$vendite]
}
if (class(data_test$vendite) == "factor") {
  data_test$vendite <- as.numeric(levels(data_test$vendite))[data_test$vendite]
}




# ============ Random Forest: single model for all sottoarea ======================

rfs_train <- filter(dataset_polimi, anno < 2016 | giorno_anno <= (140-giorni_test))

rfs_test <- filter(dataset_polimi, anno==2016 & giorno_anno >(140-giorni_test))



# Turn some features to factors
factorVars <- c('zona','area', "sottoarea",
                'prod','giorno_mese', "giorno_settimana", "giorno_anno", "mese", "settimana_anno", "anno", "weekend","stagione", "key", "primo_del_mese", "azienda_chiusa", "cluster")


rfs_train[factorVars] <- lapply(rfs_train[factorVars], function(x) as.factor(x))
rfs_test[factorVars] <- lapply(rfs_test[factorVars], function(x) as.factor(x))

# Convert dates to class "Data"
rfs_train$data <- as.Date(rfs_train$data)
rfs_test$data <- as.Date(rfs_test$data)


# Convert "vendite" to numeric values if needed
if (class(rfs_train$vendite) == "factor") {
  rfs_train$vendite <- as.numeric(levels(rfs_train$vendite))[rfs_train$vendite]
}
if (class(rfs_test$vendite) == "factor") {
  rfs_test$vendite <- as.numeric(levels(rfs_test$vendite))[rfs_test$vendite]
}


rfs <- function(n){

  rfs_model <- ranger(rfs_train,
                       formula=vendite ~ key+ zona + area + sottoarea  + prod + giorno_settimana + giorno_mese + giorno_anno + settimana_anno + mese + anno + weekend + stagione + primo_del_mese + cluster + latitudine + longitudine,
                       num.trees = n, importance="impurity", write.forest = T)
  
  # # plot importance
  # 
  # rfs_importance <- importance(rfs_model)
  # rfs_importance_df <- data.frame(name = names(rfs_importance), rfs_importance)
  # ggplot(rfs_importance_df, aes(x = reorder(name, rfs_importance), 
  #                           y = rfs_importance)) +
  #   geom_bar(stat='identity', colour = 'black') +
  #   labs(x = 'Variables', title = 'Relative Variable Importance') +
  #   coord_flip() + 
  #   theme_few()
  
  # predict
  
  rfs_predict <- predict(rfs_model, rfs_test)
  
  return(rfs_predict)

}

# number of trees to use
n <- 400

# get the prediction
rfs_prediction <- rfs(n)

# # get sse
# 
# rfs_sse <- (1/nrow(rfs_test))*sum((rfs_test$vendite - rfs_prediction$predictions)^2)
# print(rfs_sse)

# requires "scoring functions.R"
maxape(rfs_test$vendite, rfs_prediction$predictions)
meanape(rfs_test$vendite, rfs_prediction$predictions)

# ============ Random Forest: Multiple models for different sottoareas ==============

# k is the key of the location
# n is the number of trees to spawn
rfm <- function(k,n){
  
  rfm_train <- filter(dataset_polimi, anno < 2016 | giorno_anno <= (140-giorni_test), key==k)
  
  rfm_test <- filter(dataset_polimi, anno==2016 & giorno_anno >(140-giorni_test), key==k)
  
  # Turn some features to factors
  factorVars <- c('zona','area', "sottoarea",
                  'prod','giorno_mese', "giorno_settimana", "giorno_anno", "mese", "settimana_anno", "anno", "weekend","stagione", "key", "primo_del_mese", "azienda_chiusa", "cluster")
  
  rfm_train[factorVars] <- lapply(rfm_train[factorVars], function(x) as.factor(x))
  rfm_test[factorVars] <- lapply(rfm_test[factorVars], function(x) as.factor(x))
  
  # Convert dates to class "Data"
  rfm_train$data <- as.Date(rfm_train$data)
  rfm_test$data <- as.Date(rfm_test$data)
  
  
  # Convert "vendite" to numeric values if needed
  if (class(rfm_train$vendite) == "factor") {
    rfm_train$vendite <- as.numeric(levels(rfm_train$vendite))[rfm_train$vendite]
  }
  if (class(rfm_test$vendite) == "factor") {
    rfm_test$vendite <- as.numeric(levels(rfm_test$vendite))[rfm_test$vendite]
  }
  
  
  rfm_model <- ranger(rfm_train,
                      formula=vendite ~ prod + giorno_settimana + giorno_mese + giorno_anno + settimana_anno + mese + anno + weekend + stagione + primo_del_mese + cluster + latitudine + longitudine,
                      num.trees = n, importance="impurity", write.forest = T)
  
  # # plot importance
  # 
  # rfm_importance <- importance(rfm_model)
  # rfm_importance_df <- data.frame(name = names(rfm_importance), rfm_importance)
  # ggplot(rfm_importance_df, aes(x = reorder(name, rfm_importance), 
  #                               y = rfm_importance)) +
  #   geom_bar(stat='identity', colour = 'black') +
  #   labs(x = 'Variables', title = 'Relative Variable Importance') +
  #   coord_flip() + 
  #   theme_few()
  
  # predict
  
  rfm_predict <- predict(rfm_model, rfm_test)
  
  # # get sse
  # 
  # rfm_sse <- (1/nrow(rfm_test))*sum((rfm_test$vendite - rfm_predict$predictions)^2)
  
  return(rfm_predict)
}
# num trees
n<- 400

# join the prediction vectors into a single vector
rfm_prediction_global <- vector(mode="numeric", length=0)
for(i in unique(dataset_polimi$key)){
  temp <- rfm(i,n)
  rfm_prediction_global <- c(rfm_prediction_global, temp$predictions)
}

# # get sse
# rfm_sse <- (1/nrow(data_test))*sum((data_test$vendite - rfm_prediction_global)^2)
# 
# print(rfm_sse)

# requires "scoring functions.R"
maxape(data_test$vendite, rfm_prediction_global)
meanape(data_test$vendite, rfm_prediction_global)
# ======= EXTRA

# k is the key of an subarea
# p is prediction vector (rfm_prediction_global or rfs_prediction$predictions)

key_analysis <- function(k, p){
  temp_data <- cbind(data_test, p)
  names(temp_data)[names(temp_data) == 'p'] <- 'pred'
  temp_data <- filter(temp_data, key==k)
  for(p in unique(temp_data$prod)){
    temp_data_p <- filter(temp_data, prod==p)
    plot.ts(as.ts(temp_data_p$pred, order.by = index(temp_data_p$pred)), col="red", ylim=c(0, 12), main=paste("Chiave", as.character(k), "Prodotto", as.character(p)))
    lines(as.ts(temp_data_p$vendite, order.by = index(temp_data_p$vendite)), col="green")
  }
  temp_sse <- (1/nrow(temp_data))*sum((coredata(temp_data$vendite) - temp_data$pred)^2)
  return(temp_sse)
}


# sse_vector_rfs <- vector(mode="numeric", length=0)
# for (k in unique(data_test$key)){
#   sse_vector_rfs <- c(sse_vector_rfs, key_analysis(k, rfs_prediction$predictions))
# }
# 
# sse_vector_rfm <- vector(mode="numeric", length=0)
# for (k in unique(data_test$key)){
#   sse_vector_rfm <- c(sse_vector_rfm, key_analysis(k, rfm_prediction_global))
# }
# 



