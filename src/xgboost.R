library(dplyr)
library(xgboost)

# DATA IMPORT AND CLEANING
# ------------------------------------------------------
# ------------------------------------------------------

setwd("~/DM-Project")
dataset <- read.csv("~/DM-Project/Modified data/dataset_polimi_final_vendite_giorn.csv", stringsAsFactors=FALSE, row.names=NULL)
# Remove the x column, if present
if("X" %in% names(dataset)){
  dataset <- arrange(dataset, X)
}
dataset <- dataset[ , !(names(dataset) %in% c("X"))]

# Build a smaller datasetset, for testing
# dataset <- dataset[sample(1:nrow(dataset), 1000), ]

# Convert dates to class "Data"
dataset$data <- as.Date(dataset$data)
# Convert "vendite" to numeric values if needed
if (class(dataset$vendite) == "factor") {
  dataset$vendite <- as.numeric(levels(dataset$vendite))[dataset$vendite]
}

# Turn some features to factors
factorVars <- c('zona','area', "sottoarea",
                'prod','giorno_mese', "giorno_settimana", "giorno_anno", "mese", "settimana_anno", "anno", "weekend","stagione", "key", "primo_del_mese")

dataset[factorVars] <- lapply(dataset[factorVars], function(x) as.factor(x))

data_train <- filter(dataset, as.numeric(as.character(anno)) < 2016 | as.numeric(as.character(giorno_anno)) <= 130)

data_test <- filter(dataset, as.numeric(as.character(anno))==2016 & as.numeric(as.character(giorno_anno)) >130)



# ---------------------------------------
# Use Single XGBoost 
# ---------------------------------------
xg_single <- function(n){
  
  xg_train <- xgb.DMatrix(model.matrix(~zona + area + sottoarea + prod + giorno_mese + giorno_settimana + giorno_anno + mese + settimana_anno + anno + weekend + stagione + key + primo_del_mese, data=data_train),
                          label=data_train$vendite, missing=NA)
  xg_test <- xgb.DMatrix(model.matrix(~zona + area + sottoarea + prod + giorno_mese + giorno_settimana + giorno_anno + mese + settimana_anno + anno + weekend + stagione + key + primo_del_mese, data=data_test),
                         label=data_test$vendite, missing=NA)
  
  xgb_model <- xgboost(xg_train, data_train$vendite, nrounds = n)
  xgb_pred <- predict(xgb_model, xg_test)
  return(xgb_pred)

}
num_rounds <- 90
xgb_pred <- xg_single(num_rounds)
print(c("MSE", mse(data_test$vendite, xgb_pred)))
print(c("MeanAPE", meanape(data_test$vendite, xgb_pred)))
print(c("MaxAPE", maxape(data_test$vendite, xgb_pred)))


#-----------------------
# Use multiple XGBoost
# assuming dataset is sorted in ascending ascending: zona, area, sottoarea, prodotto, data
#-----------------------

xg_multi <- function(n, k){
  temp_train <- filter(data_train, key==k)
  temp_test <- filter(data_test, key==k)
  xg_train <- xgb.DMatrix(model.matrix(~prod + giorno_mese + giorno_settimana + giorno_anno + mese + settimana_anno + anno + weekend + stagione + primo_del_mese, data=temp_train),
                          label=temp_train$vendite, missing=NA)
  xg_test <- xgb.DMatrix(model.matrix(~prod + giorno_mese + giorno_settimana + giorno_anno + mese + settimana_anno + anno + weekend + stagione + primo_del_mese, data=temp_test),
                         label=temp_test$vendite, missing=NA)
  
  xgb_model <- xgboost(xg_train, temp_train$vendite, nrounds = n)
  xgb_pred <- predict(xgb_model, xg_test)
  return(xgb_pred)
}

n<- 90
xgb_pred_global <- vector(mode="numeric", length=0)
for(i in unique(data_test$key)){
  temp <- xg_multi(n, i)
  xgb_pred_global <- c(xgb_pred_global, temp)
}

print(c("MSE", mse(data_test$vendite, xgb_pred_global)))
print(c("MeanAPE", meanape(data_test$vendite, xgb_pred_global)))
print(c("MaxAPE", maxape(data_test$vendite, xgb_pred_global)))
