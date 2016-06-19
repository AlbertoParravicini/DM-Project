library(dplyr)
library(xgboost)
library(ggthemes) # visualization
source("src/scoring functions.R")
source("src/sarima.R")
library(stringr)
library(xts)
library(ggplot2)
library(dplyr)
library(tseries)
library(PerformanceAnalytics)
library(forecast)
library(astsa)
library(Metrics)
library(Ckmeans.1d.dp)
library(DiagrammeR)

setClass(Class = "xgboost_pred", representation(predictions = "numeric", prediction_table = "data.frame",
                                                sse = "numeric", mape = "numeric", maxape="numeric"))
setClass(Class = "full_xgboost_pred", representation(predictions = "data.frame", sse_list = "numeric"))

# dataset <- read.csv("~/DM-Project/Modified data/dataset_polimi_with_holidays.csv", stringsAsFactors=FALSE, row.names=NULL)

dataset <- dataset_polimi_with_holidays

prediction_length <- 10

# factorVars <- c('zona','area', "sottoarea",'prod','giorno_mese', "giorno_settimana", "giorno_anno",
#                 "mese", "settimana_anno", "anno", "weekend","stagione", "key", "primo_del_mese",
#                 "azienda_chiusa", "cluster3", "cluster6", "cluster20", "vacanza")



# dataset[factorVars] <- lapply(dataset[factorVars], function(x) as.factor(x))

# Convert dates to class "Data"
dataset$data <- as.Date(dataset$data, format = "%Y-%m-%d")

data_train <- filter(dataset, data <= max(data) - prediction_length)
data_test <- filter(dataset, data > max(data) - prediction_length)


data_train$data <- as.Date(data_train$data, format = "%Y-%m-%d")
data_test$data <- as.Date(data_test$data, format = "%Y-%m-%d")

# Convert "vendite" to numeric values if needed
if (class(dataset$vendite) == "factor") {
  dataset$vendite <- as.numeric(levels(dataset$vendite))[dataset$vendite]
}
if (class(data_train$vendite) == "factor") {
  data_train$vendite <- as.numeric(levels(data_train$vendite))[data_train$vendite]
}
if (class(data_test$vendite) == "factor") {
  data_test$vendite <- as.numeric(levels(data_test$vendite))[data_test$vendite]
}


# ########### BEGIN VENDITE GIORNALIERE PRODOTTO #########################
# 
# # get sarima predictions for the product days
# prod_1_daily <- pred_test_regressors(prediction_start = (max(dataset$data) - prediction_length +1),
#                                      prediction_length=prediction_length, num_prod=1)
# prod_2_daily <- pred_test_regressors(prediction_start = (max(dataset$data) - prediction_length +1),
#                                      prediction_length=prediction_length, num_prod=2)
# 
# data_test <-data_test[, !(names(data_test) %in% c("vendite_giorn_prod", "vendite_giorn_tot"))]
# data_test_p_1 <- filter(data_test, prod == 1)
# data_test_p_1$data <- as.Date(data_test_p_1$data, format = "%Y-%m-%d")
# data_test_p_2 <- filter(data_test, prod == 2)
# data_test_p_2$data <- as.Date(data_test_p_2$data, format = "%Y-%m-%d")
# 
# # prod_1_daily$data <- as.numeric(as.character(prod_1_daily$data))
# # prod_1_daily$data <- as.Date("1970-01-01", format="%Y-%m-%d") + prod_1_daily$data
# 
# # total_table <- merge(data_test, prod_1_daily, by = c("prod", "data"), all.x = T)
# 
# prod_1_d <- data.frame(as.vector(prod_1_daily), as.Date(index(prod_1_daily)))
# names(prod_1_d)[names(prod_1_d)=="as.Date.index.prod_1_daily.."] <- "data"
# names(prod_1_d)[names(prod_1_d)=="as.vector.prod_1_daily."] <- "vendite_giorn_prod"
# prod_2_d <- data.frame(as.vector(prod_2_daily), as.Date(index(prod_2_daily)))
# names(prod_2_d)[names(prod_2_d)=="as.Date.index.prod_2_daily.."] <- "data"
# names(prod_2_d)[names(prod_2_d)=="as.vector.prod_2_daily."] <- "vendite_giorn_prod"
# 
# 
# data_test_p_1 <- left_join(data_test_p_1, prod_1_d)
# # names(data_test_p_1)[names(data_test_p_1)=="prod_1_daily"] <- "vendite_giorn_prod"
# data_test_p_2 <- left_join(data_test_p_2, prod_2_d)
# # names(data_test_p_2)[names(data_test_p_2)=="prod_2_daily"] <- "vendite_giorn_prod"
# data_test <- rbind(data_test_p_1, data_test_p_2)
# 
# ########## END VENDITE GIORNALIERE PRODOTTO ##################


# ========================================================================================
# ========================================================================================
# ============================XGBoost model builder ======================================
# ========================================================================================
# ========================================================================================
# INFOS:
# - n_rounds is the number of decision trees in the final model
# - to do cross validation use xgb.cv instead of xgb.train an specify nfold value
# - gradient boosting algorithm
# - early.stop.round = X to terminate training process if performance is getting worse
#   for X steps (e.g. X=3)

xg_single <- function(n_rounds=45, details=F){
  
  # build datasets
  xg_train <- xgb.DMatrix(model.matrix(~zona + area + sottoarea  + prod + giorno_settimana +
                                         giorno_mese + giorno_anno + settimana_anno + mese + anno + weekend 
                                          + primo_del_mese + cluster3 + cluster6 + cluster20 + 
                                         latitudine + longitudine + vacanza, data=data_train),
                          label=data_train$vendite, missing=NA)
  xg_test <- xgb.DMatrix(model.matrix(~zona + area + sottoarea  + prod + giorno_settimana +
                                        giorno_mese + giorno_anno + settimana_anno + mese + anno + weekend 
                                         + primo_del_mese + cluster3 + cluster6 + cluster20 + 
                                        latitudine + longitudine + vacanza, data=data_test),
                         label=data_test$vendite, missing=NA)
  # removed stagione!
  
  watchlist <- list(train=xg_train, test=xg_test)
  
  # build model
  xgb_model <- xgb.train(data=xg_train, nrounds = n_rounds, nthread = 4, 
                         watchlist=watchlist, eta = 0.095)
  xgb_pred <- predict(xgb_model, xg_test)

  # get some scoring
  sse <- (1/nrow(data_test))*sum((xgb_pred - data_test$vendite)^2)
  mape <- mean(abs(xgb_pred - data_test$vendite)/mean(data_test$vendite))
  maxape <- max(abs(xgb_pred - data_test$vendite)/mean(data_test$vendite))
  if (details) {
    cat("SSE: ", sse, "\n")
    cat("MAPE: ", mape , "\n")
    cat("MAX APE: ", maxape, "\n")
  }  
  
  # if(details){
  #   importance_matrix <- xgb.importance(model = xgb_model)
  #   print(importance_matrix)
  #   xgb.plot.importance(importance_matrix = importance_matrix)
  #   
  #   xgb.plot.tree(model = xgb_model)
  # }
  
  data_test[, "vendite"] <- xgb_pred
  return(new("xgboost_pred", predictions = xgb_pred, prediction_table = data_test, sse = sse, mape = mape, maxape = maxape))

}

xgb_pred <- xg_single(n_rounds=650,details=T)

# eta 0.2 rounds = 250
# SSE:  2.009147 
# MAPE:  0.276284 
# MAX APE:  3.992809 

# eta 0.1 nrounds=667
# SSE:  1.904172 
# MAPE:  0.2708243 
# MAX APE:  4.035991 

# eta 0.095 nrounds 700
# SSE:  1.923633 
# MAPE:  0.2686256 
# MAX APE:  4.051457 

# eta 0.09 nrounds = 780
# SSE:  1.982959 
# MAPE:  0.2767076 
# MAX APE:  4.065648 

# eta 0.075 nrounds =780
# SSE:  1.993004 
# MAPE:  0.2746128 
# MAX APE:  4.059773 

# eta 0.05 nrounds 1123
# SSE:  1.974349 
# MAPE:  0.2726675 
# MAX APE:  4.037817 