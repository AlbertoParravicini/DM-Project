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

LogLoss<-function(actual, predicted)
{
  predicted<-(pmax(predicted, 0.00001))
  predicted<-(pmin(predicted, 0.99999))
  result<- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
  return(result)
}

setClass(Class = "xgboost_pred", representation(predictions = "numeric", prediction_table = "data.frame",
                                                sse = "numeric", mape = "numeric", maxape="numeric"))
setClass(Class = "full_xgboost_pred", representation(predictions = "data.frame", sse_list = "numeric"))

# used to fastly rerun the algorithm

dataset <- read.csv("~/DM-Project/Modified data/dataset_polimi_final_with_holidays_v2.csv", stringsAsFactors=FALSE, row.names=NULL)
  
#dataset <- dataset_polimi_with_holidays
  
prediction_length <- 10
  
factorVars <- c('zona','area', "sottoarea",'prod','giorno_mese', "giorno_settimana", "giorno_anno",
                "mese", "settimana_anno", "anno", "weekend","stagione", "key", "primo_del_mese",
                "vendite_missing", "cluster3", "cluster6", "cluster20", "vacanza")


  # dataset <- read.csv("Modified data/dataset_polimi_with_holidays.csv", stringsAsFactors=FALSE, row.names=NULL)
  
  dataset <- dataset_polimi_final_with_holidays_v2
  dataset$stagione[dataset$stagione=="inverno"] <- 1
  dataset$stagione[dataset$stagione=="primavera"] <- 2
  dataset$stagione[dataset$stagione=="estate"] <- 3
  dataset$stagione[dataset$stagione=="autunno"] <- 4
  dataset$stagione <- as.numeric(dataset$stagione)
  prediction_length <- 10

  
  
  
dataset[factorVars] <- lapply(dataset[factorVars], function(x) as.factor(x))
  
# Convert dates to class "Data"
dataset$data <- as.Date(dataset$data, format = "%Y-%m-%d")
  

data_train <- filter(dataset, data <= max(data) - prediction_length)
data_test <- filter(dataset, data > max(data) - prediction_length)

# TOGLI DAL DATASET SOTTOAREE+PROD DA NON PREDIRRE

  # remove sottoarea 20
  dataset <- filter(dataset, sottoarea!=20)
  # remove sottoarea 78 prodotto 2
  temp <- filter(dataset, sottoarea==78, prod==1)
  dataset <- filter(dataset, sottoarea!=78)
  dataset <- rbind(dataset, temp)
  # remove sottoarea 30 prodotto 2
  temp <- filter(dataset, sottoarea==32, prod==1)
  dataset <- filter(dataset, sottoarea!=32)
  dataset <- rbind(dataset, temp)
  
  
  

  # Convert dates to class "Data"
  dataset$data <- as.Date(dataset$data, format = "%Y-%m-%d")

  
  
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


###################################################################################################
###################################################################################################
# ======================================= ONE XGBOOST  ========================================== #
###################################################################################################
###################################################################################################
# INFOS:
# - n_rounds is the number of decision trees in the final model
# - to do cross validation use xgb.cv instead of xgb.train an specify nfold value
# - gradient boosting algorithm
# - early.stop.round = X to terminate training process if performance is getting worse
#   for X steps (e.g. X=3)
xg_single <- function(n_rounds=45, details=F){
  
  # build datasets
  xg_train <- xgb.DMatrix(model.matrix(~ zona + area + sottoarea  + prod + giorno_settimana +
                                         giorno_mese + giorno_anno + settimana_anno + mese + anno + weekend 
                                          + primo_del_mese + cluster3 + cluster6 + cluster20 + 
                                         latitudine + longitudine + vacanza + stagione, data=data_train),
                          label=data_train$vendite, missing=NA)
  xg_test <- xgb.DMatrix(model.matrix(~zona + area + sottoarea  + prod + giorno_settimana +
                                        giorno_mese + giorno_anno + settimana_anno + mese + anno + weekend 
                                         + primo_del_mese + cluster3 + cluster6 + cluster20 + 
                                        latitudine + longitudine + vacanza + stagione, data=data_test),
                         label=data_test$vendite, missing=NA)
  # removed stagione!  
  
  watchlist <- list( test=xg_test, train=xg_train)
  
  # build model
  xgb_model <- xgb.train(data=xg_train, nrounds = n_rounds, nthread = 4, verbose = T,
                         watchlist=watchlist, eta = 0.07, eval.metric="logloss", eval.metric="rmse")

  xgb_pred <- predict(xgb_model, xg_test)
  
  xgb.plot.tree(model = xgb_model)

  # get some scoring
  sse <- (1/nrow(data_test))*sum((xgb_pred - data_test$vendite)^2)
  mape <- mean(abs(xgb_pred - data_test$vendite)/mean(data_test$vendite))
  maxape <- max(abs(xgb_pred - data_test$vendite)/mean(data_test$vendite))
  logloss <- LogLoss(data_test$vendite, xgb_pred)
  if (details) {
    cat("SSE: ", sse, "\n")
    cat("MAPE: ", mape , "\n")
    cat("MAX APE: ", maxape, "\n")
    cat("LOGLOSS: ", logloss, "\n")
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

xgboost_pred <- xg_single(n_rounds=700,details=T)

# eta 0.2 rounds = 250
# SSE:  2.009147 
# MAPE:  0.276284 
# MAX APE:  3.992809 

# ========= BEST ONE =========
# eta 0.1 nrounds=900
# SSE:  1.882204 
# MAPE:  0.2682891 
# MAX APE:  4.052917 

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

###################################################################################################
###################################################################################################
# ================ ONE XGBOOST PER SOTTOAREA, WITH EARLY STOPPING OF TRAINING =================== #
###################################################################################################
###################################################################################################


xg_multi <- function(n_rounds=45, eta_value=0.1, details=F){
  
  predizioni_sottoarea <- c()
  
  for(s in unique(data_train$sottoarea)){
    cat("SOTTOAREA: ", s, "\n")
    
    data_train_area <- filter(data_train, sottoarea==s)
    data_test_area <- filter(data_test, sottoarea==s)
    
    print(data_test_area[1,])
    
    # build datasets
    xg_train <- xgb.DMatrix(model.matrix(~ prod + giorno_settimana +
                                           giorno_mese + giorno_anno + settimana_anno + mese + anno + weekend 
                                         + primo_del_mese + vacanza, data=data_train_area),
                            label=data_train_area$vendite, missing=NA)
    xg_test <- xgb.DMatrix(model.matrix(~ + prod + giorno_settimana +
                                          giorno_mese + giorno_anno + settimana_anno + mese + anno + weekend 
                                        + primo_del_mese + vacanza, data=data_test_area),
                           label=data_test_area$vendite, missing=NA)
    # removed stagione!  
    
    watchlist <- list(train=xg_train, test=xg_test)
    
    # build model
    xgb_model <- xgb.train(data=xg_train, nrounds = n_rounds, nthread = 4, watchlist = list(test=xg_test),
                            eta = eta_value, early.stop.round = 5, maximize = F)
    xgb_pred <- predict(xgb_model, xg_test)
    
    # get some scoring
    sse <- (1/nrow(data_test_area))*sum((xgb_pred - data_test_area$vendite)^2)
    mape <- mean(abs(xgb_pred - data_test_area$vendite)/mean(data_test_area$vendite))
    maxape <- max(abs(xgb_pred - data_test_area$vendite)/mean(data_test_area$vendite))
    if (details) {
      cat("SSE: ", sse, "\n")
      cat("MAPE: ", mape , "\n")
      cat("MAX APE: ", maxape, "\n")
    }  
    

    data_test_area["vendite_predette"]<-0
    data_test_area[, "vendite_predette"] <- xgb_pred
    predizioni_sottoarea<-c(predizioni_sottoarea, 
                            (new("xgboost_pred", predictions = xgb_pred, prediction_table = data_test_area, sse = sse, mape = mape, maxape = maxape)))
  }
  
  # now i should have a vector "predizioni_sottoarea" of predictions per area, should join them into "complete_prediction"
  
  complete_prediction <- c()
  
  for(prediction in predizioni_sottoarea){
    complete_prediction<-rbind(complete_prediction, prediction@prediction_table)
  }
  
  # compute statistics
  
  sse <- (1/nrow(complete_prediction))*sum((complete_prediction$vendite_predette - complete_prediction$vendite)^2)
  mape <- mean(abs(complete_prediction$vendite_predette - complete_prediction$vendite)/mean(complete_prediction$vendite))
  maxape <- max(abs(complete_prediction$vendite_predette - complete_prediction$vendite)/mean(complete_prediction$vendite))
    cat("SSE: ", sse, "\n")
    cat("MAPE: ", mape , "\n")
    cat("MAX APE: ", maxape, "\n")
    
}

xgboost_pred <- xg_multi(n_rounds=500,details=T, eta_value=0.025)

# results n_round=100, eta=0,1
# SSE:  1.989518 
# MAPE:  0.2740238 
# MAX APE:  3.87759

# result nround=100, eta= 0.05
# SSE:  1.969009 
# MAPE:  0.2730418 
# MAX APE:  3.854637 

###################################################################################################
###################################################################################################
# ========================= ONE XGBOOST WITH CROSSVALIDATION  =================================== #
###################################################################################################
###################################################################################################
# INFOS:
# - n_rounds is the number of decision trees in the final model
# - to do cross validation use xgb.cv instead of xgb.train an specify nfold value
# - gradient boosting algorithm
# - early.stop.round = X to terminate training process if performance is getting worse
#   for X steps (e.g. X=3)
xg_cross <- function(n_rounds=45, details=F){
  
  # build datasets
  xg_train <- xgb.DMatrix(model.matrix(~ zona + area + sottoarea  + prod + giorno_settimana +
                                         giorno_mese + giorno_anno + settimana_anno + mese + anno + weekend+
                                         primo_del_mese + cluster3 + cluster6 + cluster20 + 
                                         latitudine + longitudine + vacanza + stagione, data=data_train),
                          label=data_train$vendite, missing=NA)
  
  xg_test <- xgb.DMatrix(model.matrix(~zona + area + sottoarea  + prod + giorno_settimana +
                                        giorno_mese + giorno_anno + settimana_anno + mese + anno + weekend+
                                        primo_del_mese + cluster3 + cluster6 + cluster20 + 
                                        latitudine + longitudine + vacanza + stagione, data=data_test),
                         label=data_test$vendite, missing=NA)
  # removed stagione!  
  
  watchlist <- list(train=xg_train)
  
  # build model
  xgb_model <- xgb.cv(data=xg_train, nrounds = n_rounds, nthread = 4, 
                         watchlist=watchlist, eta = 0.07, nfold=10,
                      eval.metric="logloss", eval.metric="rmse", eval.metric="map", 
                      tree_method="exact")
  return(xgb_model)
  # xgb_pred <- predict(xgb_model, xg_test)
  
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

xgboost_pred <- xg_cross(n_rounds=700,details=T)

aggiungi_sottoarea_prodotto <- function(dataset, sottoarea, prodotto, valore=0){
  temp <- filter(dataset, sottoarea==dataset[1,"sottoarea"], prod==dataset[1,"prod"])
  temp$sottoarea <- sottoarea
  temp$prod <- prodotto
  temp$vendite <- valore
  return(rbind(dataset, temp))
}

# hardcoda i valori mancanti
# NOTA: nelle tuple aggiunti gli unici valori corretti sono data, sottoarea, prodotto e vendite!!!!
hardcoded_test <- aggiungi_sottoarea_prodotto(xgboost_pred@prediction_table, sottoarea=20, prodotto=1, valore=0)
hardcoded_test <- aggiungi_sottoarea_prodotto(hardcoded_test, sottoarea=20, prodotto=2, valore=0)
hardcoded_test <- aggiungi_sottoarea_prodotto(hardcoded_test, sottoarea=78, prodotto=2, valore=0)
hardcoded_test <- aggiungi_sottoarea_prodotto(hardcoded_test, sottoarea=32, prodotto=2, valore=0)

# riprenditi il test set orginale senza i luoghi mancanti
data_test_2 <- filter(dataset_polimi_final_with_holidays_v2,
                      as.Date(data, format = "%Y-%m-%d") > (max(as.Date(data, format = "%Y-%m-%d")) - prediction_length))

# riordina le colonne
hardcoded_test <- arrange(hardcoded_test, prod, sottoarea, data)
data_test_2 <- arrange(data_test_2, prod, sottoarea, data)

# calcola le statistiche
sse <- (1/nrow(data_test_2))*sum((hardcoded_test$vendite - data_test_2$vendite)^2)
mape <- mean(abs(hardcoded_test$vendite - data_test_2$vendite)/mean(data_test_2$vendite))
maxape <- max(abs(hardcoded_test$vendite - data_test_2$vendite)/mean(data_test_2$vendite))
logloss <- LogLoss(data_test_2$vendite, hardcoded_test$vendite)
  
  cat("SSE: ", sse, "\n")
  cat("MAPE: ", mape , "\n")
  cat("MAX APE: ", maxape, "\n")
  cat("LOGLOSS: ", logloss, "\n")

  
#======================== STATISTICHE XGBOOST
  results <- data.frame(matrix(NA, ncol=6, nrow=0))
  
  prediction <- xgboost_pred@prediction_table
  for (s in unique(prediction$sottoarea)){
    local_test <- filter(data_test, sottoarea==s)
    local_pred <- filter(prediction, sottoarea==s)
    for (p in unique(local_test$prod)){
      sse <- (1/nrow(filter(local_test, prod==p))*sum((filter(local_pred, prod==p)$vendite - filter(local_test, prod==p)$vendite)^2))
      mape <- mean(abs(filter(local_pred, prod==p)$vendite - filter(local_test, prod==p)$vendite)/mean(filter(local_test, prod==p)$vendite))
      maxape <- max(abs(filter(local_pred, prod==p)$vendite - filter(local_test, prod==p)$vendite)/mean(filter(local_test, prod==p)$vendite))
      logloss <- LogLoss(filter(local_test, prod==p)$vendite, filter(filter(local_pred, prod==p), prod==p)$vendite)
      
      temp_row <- data.frame(sottoarea=s, prod=p, sse=sse, mape=mape, maxape=maxape, logloss=logloss )
      results <- rbind(results, temp_row)
    }
  }

  write.csv(results, file="Results/risultati_xgboost_no[20(1-2),78(2),32(2)].csv", row.names=FALSE)
  