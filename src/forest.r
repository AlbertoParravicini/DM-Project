library(xts)
library(ggplot2)
library(dplyr)
library(tseries)
library(PerformanceAnalytics)
library(forecast)
library(astsa)
library(Metrics)
library(xgboost)
library(ranger)
library(ggthemes) # visualization
source("src/scoring functions.R")
library(stringr)

setClass(Class = "forest_pred", representation(predictions = "numeric", prediction_table = "data.frame", sse = "numeric"))
setClass(Class = "full_forest_pred", representation(predictions = "data.frame", sse_list = "numeric"))


dataset <- read.csv("Modified data/dataset_polimi_final_with_holidays_v2.csv", stringsAsFactors=FALSE, row.names=NULL)


# dataset <- dataset_complete

prediction_length <- 10




# Convert dates to class "Data"
dataset$data <- as.Date(dataset$data, format = "%Y-%m-%d")
data_train$data <- as.Date(data_train$data, format = "%Y-%m-%d")
data_test$data <- as.Date(data_test$data, format = "%Y-%m-%d")

data_train <- filter(dataset, data <= max(data) - prediction_length)
data_test <- filter(dataset, data > max(data) - prediction_length)

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




# ============ Random Forest: single model for all sottoarea ======================

rfs_train <- filter(dataset, data <= max(data) - prediction_length)

rfs_test <- filter(dataset, data > max(data) - prediction_length)



# Turn some features to factors
factorVars <- c('zona','area', "sottoarea",
                'prod','giorno_mese', "giorno_settimana", "giorno_anno", "mese", "settimana_anno", "anno", "weekend","stagione", "key", "primo_del_mese", "azienda_chiusa", "cluster3", "cluster6", "cluster20", "vacanza")


rfs_train[factorVars] <- lapply(rfs_train[factorVars], function(x) as.factor(x))
rfs_test[factorVars] <- lapply(rfs_test[factorVars], function(x) as.factor(x))

# Convert dates to class "Data"
rfs_train$data <- as.Date(rfs_train$data, format = "%Y-%m-%d")
rfs_test$data <- as.Date(rfs_test$data, format = "%Y-%m-%d")


# Convert "vendite" to numeric values if needed
if (class(rfs_train$vendite) == "factor") {
  rfs_train$vendite <- as.numeric(levels(rfs_train$vendite))[rfs_train$vendite]
}
if (class(rfs_test$vendite) == "factor") {
  rfs_test$vendite <- as.numeric(levels(rfs_test$vendite))[rfs_test$vendite]
}

summary(rfs_train)
summary(rfs_test)


# ========================================================================================
# ========================================================================================
# Ranger random forest model builder =====================================================
# ========================================================================================
# ========================================================================================
rfs <- function(train_set, test_set, num_trees = 400, details = F, ...){

  prediction_length <- length(unique(test_set$data))
  
  train_set <- filter(train_set)
  test_set <- filter(test_set)
  

  rfs_model <- ranger(train_set,
                       formula=vendite ~ (zona + area + sottoarea  + prod +
                        giorno_mese + giorno_anno + settimana_anno + mese + anno  +
                        stagione + primo_del_mese + cluster3 + cluster6 + cluster20 +
                        latitudine + longitudine + giorno_settimana+weekend+vacanza),
                       num.trees = num_trees, write.forest = T, verbose = details, num.threads = 4, ...)
 
  cat("RSQUARED: ", rfs_model$r.squared, "\n")
  cat("OUT OF BAG ERROR: ", rfs_model$prediction.error, "\n")
  
  
  # 
  # # plot importance
  # if (details) {
  #   rfs_importance <- importance(rfs_model)
  #   rfs_importance_df <- data.frame(name = names(rfs_importance), rfs_importance)
  #   plot <- ggplot(rfs_importance_df, aes(x = reorder(name, rfs_importance),
  #                             y = rfs_importance)) +
  #     geom_bar(stat='identity', colour = 'black') +
  #     labs(x = 'Variables', title = 'Relative Variable Importance') +
  #     coord_flip() +
  #     theme_few()
  #   print(plot)
  #   print(rfs_model)
  # }
  
  # predict
  rfs_predict <- predict(rfs_model, test_set)
  
  
  
  
  sse <- (1/nrow(test_set))*sum((rfs_predict$predictions - test_set$vendite)^2)
  if (details) {
    cat("SSE: ", sse, "\n")
    cat("MAPE: ", mean(abs(rfs_predict$predictions - test_set$vendite)/mean(test_set$vendite)), "\n")
    cat("MAX APE: ", max(abs(rfs_predict$predictions - test_set$vendite)/mean(test_set$vendite)), "\n")
  }
  
  # if (details) {
  #   pred_table <- data.frame(vendite=rfs_predict$predictions, data=seq.Date(from=max(train_set$data)+1, length.out = prediction_length, by = 1), type = "pred")
  # 
  #   table_tot <- rbind(data.frame(vendite = train_set[, "vendite"], data = train_set[, "data"], type = "train"), data.frame(vendite = test_set[, "vendite"], data = test_set[, "data"], type = "test"), pred_table)
  # 
  #   p <- ggplot(table_tot, aes(x=data, y=vendite, color = type)) +
  #     coord_cartesian(xlim = c(max(train_set$data)-prediction_length, max(train_set$data)+prediction_length))
  #   p <- p + geom_line(size = 1) + geom_point(size = 2) + scale_colour_colorblind()
  #   p <- p + theme_economist() +xlab("Data") + ylab("Numero di vendite")
  #   print(p)
  # }
  # 
  test_set[, "vendite"] <- rfs_predict$predictions
  return(new("forest_pred", predictions = rfs_predict$predictions, prediction_table = test_set, sse = sse))
}



# ============ Random Forest: Multiple models for different subareas ==============
rfm <- function(dataset, predicion_set = NA, prediction_length = 10, num_sottoarea = 1, num_prod = 0, num_trees = 400, details = F, ...){
  
  # If we aren't given a prediction set, split the dataset according to the prediction length:
  # predict over the last "prediction_length" days.
  # Else, train the model using every data, then predict using the prediction_set
  # If num_prod != 0, build models for single products. 
  # If num_prod = 0, build models for multiple product and use the product as a splitting variable.
  if (num_prod != 0) {
    if (is.na(predicion_set)) {
      rfm_train <- filter(dataset, data <= max(data) - prediction_length, sottoarea == num_sottoarea, prod == num_prod)
      rfm_test <- filter(dataset, data > max(data) - prediction_length, sottoarea == num_sottoarea, prod == num_prod)
    }
    else {
      rfm_train <- filter(dataset, sottoarea == num_sottoarea, prod == num_prod)
      rfm_test <- filter(predicion_set, sottoarea == num_sottoarea, prod == num_prod)
      if (nrow(rfm_test) < prediction_length) {
        stop("The prediction set is too small!")
      }
    }
  }
  else {
    if (is.na(predicion_set)) {
      rfm_train <- filter(dataset, data <= max(data) - prediction_length, sottoarea == num_sottoarea)
      rfm_test <- filter(dataset, data > max(data) - prediction_length, sottoarea == num_sottoarea)
    }
    else {
      rfm_train <- filter(dataset, sottoarea == num_sottoarea)
      rfm_test <- filter(predicion_set, sottoarea == num_sottoarea)
      if (nrow(rfm_test) < prediction_length) {
        stop("The prediction set is too small!")
      }
    }
  }
  
  
  # Turn some features to factors
  factorVars <- c('zona','area', "sottoarea",
                  'prod','giorno_mese', "giorno_settimana", "giorno_anno", "mese", 
                  "settimana_anno", "anno", "weekend","stagione", "key", "primo_del_mese",
                  "azienda_chiusa", "cluster3", "cluster6", "cluster20", "vacanza")
  
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
                      formula=vendite ~ zona + area + sottoarea  + prod + giorno_settimana +
                        giorno_mese + giorno_anno + settimana_anno + mese + anno + weekend +
                        stagione + primo_del_mese + cluster3 + cluster6 + cluster20 +
                        latitudine + longitudine + vacanza, importance = "impurity",
                      num.trees = num_trees, write.forest = T, verbose = details, num.threads = 4, ...)

  
  # predict
  rfm_predict <- predict(rfm_model, rfm_test)
  
  # If negative values are predicted, round them to zero.
  rfm_predict$predictions <- ifelse(rfm_predict$predictions < 0, 0, rfm_predict$predictions)
  
  # get sse
  sse <- (1/nrow(rfm_test))*sum((rfm_test$vendite - rfm_predict$predictions)^2)
  rfm_test[, "vendite"] <- rfm_predict$predictions
  return(new("forest_pred", predictions = rfm_predict$predictions, prediction_table = rfm_test, sse = sse))
  
}


full_forest_prediction <- function(dataset, predicion_set = NA, prediction_length = 10, num_trees = 400, split_on_prod = T, details = F, ...) {

  # Turn some features to factors
  factorVars <- c('zona','area', "sottoarea",
                  'prod','giorno_mese', "giorno_settimana", "giorno_anno", "mese", 
                  "settimana_anno", "anno", "weekend","stagione", "key", "primo_del_mese",
                  "azienda_chiusa", "cluster3", "cluster6", "cluster20", "vacanza")
  
  dataset[factorVars] <- lapply(dataset[factorVars], function(x) as.factor(x))
  
  # Convert dates to class "Data"
  dataset$data <- as.Date(dataset$data, format = "%Y-%m-%d")

  # Convert "vendite" to numeric values if needed
  if (class(dataset$vendite) == "factor") {
    dataset$vendite <- as.numeric(levels(dataset$vendite))[dataset$vendite]
  }
  
  # Build a model for each product and subarea, put the results in a table
  res_frame <- data.frame(matrix(NA, ncol = ncol(dataset), nrow = 0))
  sse_list <- c()
  
  counter <- 0
  for (prod_i in 1:ifelse(split_on_prod == T, 2, 1)) {
    for(sottoarea_i in sort(unique(dataset$sottoarea))){
      if (split_on_prod != 0) {
        cat("\nNUMERO PRODOTTO: ", prod_i, "\n")
      }
      cat("SOTTOAREA: ", sottoarea_i, " - Percentage: ", 100*counter/(length(unique(dataset$sottoarea))*ifelse(split_on_prod == T, 2, 1)), "%\n")
      temp_res <- rfm(dataset = dataset, prediction_length = prediction_length, num_prod = ifelse(split_on_prod == T, prod_i, 0), num_sottoarea = sottoarea_i, num_trees = num_trees, details = T, ...)
      cat("PREDIZIONI: \n", temp_res@predictions, "\n")
      cat("SSE: ", temp_res@sse, "\n")
      
      res_frame <- rbind(res_frame, temp_res@prediction_table)
      
      sse_list <- c(sse_list, temp_res@sse)
      
      counter <- counter + 1
    }
  }
  #res_frame$data <- as.numeric(as.character(res_frame$data))
  #res_frame$data <- as.Date("1970-01-01", format="%Y-%m-%d") + res_frame$data
  
  cat("\n ------------------------ \n")
  cat("FINAL RESULTS:\n")
  cat("MEAN SSE: ", mean(sse_list), "\n")
  cat("MEDIAN SSE: ", median(sse_list), "\n")
  
  return(new("full_forest_pred", predictions = res_frame, sse_list = sse_list))
}
  


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


evaluate_forest_results <- function(validation, prediction) {
  # Join the datasets based on date and subarea
  common_dates <- intersect(unique(validation$data), unique(prediction$data))
  common_subareas <- intersect(unique(validation$sottoarea), unique(prediction$sottoarea))
  
  validation <- filter(validation, data %in% common_dates, sottoarea %in% common_subareas)
  prediction <- filter(prediction, data %in% common_dates, sottoarea %in% common_subareas)
  
  validation <- validation[order(validation$prod, validation$sottoarea, validation$data), ]
  prediction <- prediction[order(prediction$prod, prediction$sottoarea, prediction$data), ]
  
  mse = mse(validation$vendite, prediction$vendite)
  ape_list = ape(validation, prediction)
  mean_ape = meanape(validation, prediction)
  max_ape = maxape(validation, prediction)
  
  return(data.frame(mse = mse, mape = mean_ape, max_ape = max_ape))
}

results <- data.frame(matrix(NA, ncol=6, nrow=0))

compute_errors <- function(prediction, test, write = F) {
  for (s in unique(prediction$sottoarea)){
    local_test <- filter(test, sottoarea==s)
    local_pred <- filter(prediction, sottoarea==s)
    for (p in unique(local_test$prod)){
      sse <- (1/nrow(filter(local_test, prod==p))*sum((filter(local_pred, prod==p)$vendite - filter(local_test, prod==p)$vendite)^2))
      mape <- mean(abs(filter(local_pred, prod==p)$vendite - filter(local_test, prod==p)$vendite)/mean(filter(local_test, prod==p)$vendite))
      maxape <- max(abs(filter(local_pred, prod==p)$vendite - filter(local_test, prod==p)$vendite)/mean(filter(local_test, prod==p)$vendite))

      temp_row <- data.frame(sottoarea=s, prod=p, sse=sse, mape=mape, maxape=maxape)
      results <- rbind(results, temp_row)
    }
  }
  if (write) {
    write.csv(results, file="Results/risultati_xgboost_no[20(1-2),78(2),32(2)].csv", row.names=FALSE)
  }
}




