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

dataset <- read.csv("~/DM-Project/Modified data/dataset_polimi_with_holidays.csv", stringsAsFactors=FALSE, row.names=NULL)

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


# ============================================
# ============================================
# Ranger random forest model builder =========
# ============================================
# ============================================
rfs <- function(train_set, test_set, num_trees = 400, details = F){

  prediction_length <- nrow(test_set)
  
  rfs_model <- ranger(train_set,
                       formula=vendite ~ zona + area + sottoarea  + prod + giorno_settimana +
                        giorno_mese + giorno_anno + settimana_anno + mese + anno + weekend +
                        stagione + primo_del_mese + cluster3 + cluster6 + cluster20 +
                        latitudine + longitudine + vacanza,
                       num.trees = num_trees, importance="impurity", write.forest = T, verbose = details, num.threads = 4)
  
  # plot importance
  if (details) {
    rfs_importance <- importance(rfs_model)
    rfs_importance_df <- data.frame(name = names(rfs_importance), rfs_importance)
    plot <- ggplot(rfs_importance_df, aes(x = reorder(name, rfs_importance),
                              y = rfs_importance)) +
      geom_bar(stat='identity', colour = 'black') +
      labs(x = 'Variables', title = 'Relative Variable Importance') +
      coord_flip() +
      theme_few()
    print(plot)
    print(rfs_model)
  }
  
  # predict
  rfs_predict <- predict(rfs_model, test_set)
  
  print(rfs_predict$predictions)
  View(test_set)
  
  sse <- (1/prediction_length)*sum(rfs_predict$predictions - test_set$vendite)^2
  if (details) {
    cat("SSE: ", sse, "\n")
    cat("MAPE: ", mean(abs(rfs_predict$predictions - test_set$vendite)/mean(test_set$vendite)), "\n")
    cat("MAX APE: ", max(abs(rfs_predict$predictions - test_set$vendite)/mean(test_set$vendite)), "\n")
    
    print((1/prediction_length)*sum(rfs_predict$predictions[11:20] - test_set$vendite[11:20])^2)
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
  print(test_set$vendite)
  return(rfs_predict)
}

# number of trees to use
n <- 400

# get the prediction
rfs_prediction <- rfs(rfs_train, rfs_test, details = T, num_trees = n)

# # get sse

rfs_sse <- (1/nrow(rfs_test))*sum((rfs_test$vendite - rfs_prediction$predictions)^2)
print(rfs_sse)

# # requires "scoring functions.R"
# maxape(rfs_test$vendite, rfs_prediction$predictions)
# meanape(rfs_test$vendite, rfs_prediction$predictions)

# ============ Random Forest: Multiple models for different sottoareas ==============

# k is the key of the location
# n is the number of trees to spawn
rfm <- function(dataset, predicion_set = NA, prediction_length = 10, key_i, num_trees = 400, details = F){
  

  rfm_train <- filter(dataset, data <= max(data) - prediction_length, key == key_i)

  if (is.na(predicion_set)) {
    rfm_test <- filter(dataset, data > max(data) - prediction_length, key == key_i)
  }
  else {
    rfm_test <- filter(predicion_set, key == key_i)
    if (nrow(rfs_test) < prediction_length) {
      stop("The prediction set is too small!")
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
                        latitudine + longitudine + vacanza,
                      num.trees = num_trees, importance="impurity", write.forest = T, verbose = details, num.threads = 4)
  
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
n <- 400
# predicion_length
prediction_length <- 10

# join the prediction vectors into a single vector
rfm_prediction_global <- vector(mode="numeric", length=0)
c = 1
res_frame <- data.frame(matrix(NA, ncol = 4, nrow = 0))
for(i in sort(unique(dataset$key))[1:1]){
  cat("Current key: ", i, " - Percentage: ", 100*c/length(unique(dataset$key)), "%\n")
  temp <- rfm(dataset = dataset, prediction_length = prediction_length, key = i, num_trees = n, details = T)
  print(temp$predictions)
  
  prod_1_pred <- temp$predictions[1:prediction_length]
  prod_2_pred <- temp$predictions[(prediction_length + 1):length(temp$predictions)]
 
  temp_row_1 <- cbind(prod = 1, sottoarea = i, data = seq.Date(from = max(dataset$data)-prediction_length, length.out = prediction_length, by = 1), vendite = prod_1_pred)
  temp_row_2 <- cbind(prod = 2, sottoarea = i, data = seq.Date(from = max(dataset$data)-prediction_length, length.out = prediction_length, by = 1), vendite = prod_2_pred)
  
  res_frame <- rbind(res_frame, temp_row_1, temp_row_2)
  
  rfm_prediction_global <- c(rfm_prediction_global, temp$predictions)
  c <- c + 1
}

res_frame$data <- as.numeric(as.character(res_frame$data))
res_frame$data <- as.Date("1970-01-01", format="%Y-%m-%d") + res_frame$data



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



