library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm
library(ranger) # faster version of randomForest
library(nnet) # For binarization
library(zoo) # For time series


dataset <- read.csv("~/DM-Project/Modified data/dataset_polimi_final_with_holidays_v2.csv", row.names=NULL, stringsAsFactors=FALSE)
prediction_length <- 10


dataset <- dataset[ , !(names(dataset) %in% c("X"))]

# Convert dates to class "Data"
dataset$data <- as.Date(dataset$data)
# Convert "vendite" to numeric values if needed
if (class(dataset$vendite) == "factor") {
  dataset$vendite <- as.numeric(levels(dataset$vendite))[dataset$vendite]
}

dataset <- dataset[order(dataset$prod, dataset$sottoarea, dataset$data), ]

train <- filter(dataset, data <= max(data) - prediction_length)
test <- filter(dataset, data > max(data) - prediction_length)

test_final <- test


random_forest_TEST <- read.csv("~/DM-Project/Results/predizione_random_forest_TEST.csv", row.names=NULL, stringsAsFactors=FALSE)
sarima_TEST <- read.csv("~/DM-Project/Results/predizione_sarima_TEST.csv", row.names=NULL, stringsAsFactors=FALSE)
xgboost_TEST <- read.csv("~/DM-Project/Results/predizione_xgboost_TEST.csv", row.names=NULL, stringsAsFactors=FALSE)

random_forest_pesi <- read.csv("~/DM-Project/Results/pesi_random_forest.csv", row.names=NULL, stringsAsFactors=FALSE)
sarima_pesi <- read.csv("~/DM-Project/Results/pesi_sarima.csv", row.names=NULL, stringsAsFactors=FALSE)
xgboost_pesi <- read.csv("~/DM-Project/Results/pesi_xgboost.csv", row.names=NULL, stringsAsFactors=FALSE)

random_forest_pesi <- random_forest_pesi[order(random_forest_pesi$prod, random_forest_pesi$sottoarea), ]
sarima_pesi <- sarima_pesi[order(sarima_pesi$prod, sarima_pesi$sottoarea), ]
xgboost_pesi <- xgboost_pesi[order(xgboost_pesi$prod, xgboost_pesi$sottoarea), ]

random_forest_TEST <- random_forest_TEST[order(random_forest_TEST$prod, random_forest_TEST$sottoarea, random_forest_TEST$data), ]
sarima_TEST <- sarima_TEST[order(sarima_TEST$prod, sarima_TEST$sottoarea, sarima_TEST$data), ]
xgboost_TEST <- xgboost_TEST[order(xgboost_TEST$prod, xgboost_TEST$sottoarea, xgboost_TEST$data), ]

# remove sottoarea 20
test <- filter(test, test$sottoarea != "20")
# remove sottoarea 78 prodotto 2
temp <- filter(test, sottoarea == "78", prod=="1")
test <- filter(test, sottoarea!="78")
test <- rbind(test, temp)
# remove sottoarea 30 prodotto 2
temp <- filter(test, sottoarea == "32", prod=="1")
test <- filter(test, sottoarea != "32")
test <- rbind(test, temp)

# remove sottoarea 20
random_forest_TEST <- filter(random_forest_TEST, random_forest_TEST$sottoarea != "20")
# remove sottoarea 78 prodotto 2
temp <- filter(random_forest_TEST, sottoarea=="78", prod=="1")
random_forest_TEST <- filter(random_forest_TEST, sottoarea!="78")
random_forest_TEST <- rbind(random_forest_TEST, temp)
# remove sottoarea 30 prodotto 2
temp <- filter(random_forest_TEST, sottoarea=="32", prod=="1")
random_forest_TEST <- filter(random_forest_TEST, sottoarea!="32")
random_forest_TEST <- rbind(random_forest_TEST, temp)

# remove sottoarea 20
sarima_TEST <- filter(sarima_TEST, sarima_TEST$sottoarea!="20")
# remove sottoarea 78 prodotto 2
temp <- filter(sarima_TEST, sottoarea=="78", prod=="1")
sarima_TEST <- filter(sarima_TEST, sottoarea!="78")
sarima_TEST <- rbind(sarima_TEST, temp)
# remove sottoarea 30 prodotto 2
temp <- filter(sarima_TEST, sottoarea=="32", prod=="1")
sarima_TEST <- filter(sarima_TEST, sottoarea!="32")
sarima_TEST <- rbind(sarima_TEST, temp)

# remove sottoarea 20
xgboost_TEST <- filter(xgboost_TEST, xgboost_TEST$sottoarea!="20")
# remove sottoarea 78 prodotto 2
temp <- filter(xgboost_TEST, sottoarea=="78", prod=="1")
xgboost_TEST <- filter(xgboost_TEST, sottoarea!="78")
xgboost_TEST <- rbind(xgboost_TEST, temp)
# remove sottoarea 30 prodotto 2
temp <- filter(xgboost_TEST, sottoarea=="32", prod=="1")
xgboost_TEST <- filter(xgboost_TEST, sottoarea!="32")
xgboost_TEST <- rbind(xgboost_TEST, temp)


# remove sottoarea 20
random_forest_pesi <- filter(random_forest_pesi, random_forest_pesi$sottoarea != "20")
# remove sottoarea 78 prodotto 2
temp <- filter(random_forest_pesi, sottoarea=="78", prod=="1")
random_forest_pesi <- filter(random_forest_pesi, sottoarea!="78")
random_forest_pesi <- rbind(random_forest_pesi, temp)
# remove sottoarea 30 prodotto 2
temp <- filter(random_forest_pesi, sottoarea=="32", prod=="1")
random_forest_pesi <- filter(random_forest_pesi, sottoarea!="32")
random_forest_pesi <- rbind(random_forest_pesi, temp)

# remove sottoarea 20
sarima_pesi <- filter(sarima_pesi, sarima_pesi$sottoarea!="20")
# remove sottoarea 78 prodotto 2
temp <- filter(sarima_pesi, sottoarea=="78", prod=="1")
sarima_pesi <- filter(sarima_pesi, sottoarea!="78")
sarima_pesi <- rbind(sarima_pesi, temp)
# remove sottoarea 30 prodotto 2
temp <- filter(sarima_pesi, sottoarea=="32", prod=="1")
sarima_pesi <- filter(sarima_pesi, sottoarea!="32")
sarima_pesi <- rbind(sarima_pesi, temp)

# remove sottoarea 20
xgboost_pesi <- filter(xgboost_pesi, xgboost_pesi$sottoarea!="20")
# remove sottoarea 78 prodotto 2
temp <- filter(xgboost_pesi, sottoarea=="78", prod=="1")
xgboost_pesi <- filter(xgboost_pesi, sottoarea!="78")
xgboost_pesi <- rbind(xgboost_pesi, temp)
# remove sottoarea 30 prodotto 2
temp <- filter(xgboost_pesi, sottoarea=="32", prod=="1")
xgboost_pesi <- filter(xgboost_pesi, sottoarea!="32")
xgboost_pesi <- rbind(xgboost_pesi, temp)


# Turn some features to factors
factorVars <- c('zona','area', "sottoarea",
                'prod','giorno_mese', "giorno_settimana", "giorno_anno", "vendite_missing", "mese", "settimana_anno", "anno", "weekend","stagione", "key", "primo_del_mese", "cluster3", "cluster6", "cluster20", "vacanza")

dataset[factorVars] <- lapply(dataset[factorVars], function(x) as.factor(x))





factorVars <- c("sottoarea",
                'prod')

random_forest_pesi[factorVars] <- lapply(random_forest_pesi[factorVars], function(x) as.factor(x))
sarima_pesi[factorVars] <- lapply(sarima_pesi[factorVars], function(x) as.factor(x))
xgboost_pesi[factorVars] <- lapply(xgboost_pesi[factorVars], function(x) as.factor(x))
random_forest_TEST[factorVars] <- lapply(random_forest_TEST[factorVars], function(x) as.factor(x))
sarima_TEST[factorVars] <- lapply(sarima_TEST[factorVars], function(x) as.factor(x))
xgboost_TEST[factorVars] <- lapply(xgboost_TEST[factorVars], function(x) as.factor(x))

random_forest_TEST$data <- as.Date(random_forest_TEST$data)
sarima_TEST$data <- as.Date(sarima_TEST$data)
xgboost_TEST$data <- as.Date(xgboost_TEST$data)


# Make sure xgboost doesn't predict below 0
xgboost_TEST$vendite <- ifelse(xgboost_TEST$vendite < 0, 0, xgboost_TEST$vendite)

summary(random_forest_TEST)
summary(xgboost_TEST)
summary(sarima_TEST)

summary(random_forest_pesi)
summary(xgboost_pesi)
summary(sarima_pesi)

mse_forest <- (1/nrow(random_forest_TEST))*sum((random_forest_TEST$vendite - test$vendite)^2)
mse_sarima <- (1/nrow(sarima_TEST))*sum((sarima_TEST$vendite - test$vendite)^2)
mse_xgboost <- (1/nrow(xgboost_TEST))*sum((xgboost_TEST$vendite - test$vendite)^2)

for (i in  1:nrow(test)) {
  temp_row <- test[i, ]
  xgboost_sse <- filter(xgboost_pesi, prod == temp_row$prod, sottoarea == temp_row$sottoarea)$sse
  sarima_sse <- filter(sarima_pesi, prod == temp_row$prod, sottoarea == temp_row$sottoarea)$sse
  forest_sse <- filter(random_forest_pesi, prod == temp_row$prod, sottoarea == temp_row$sottoarea)$sse
  
  
  test$predizione[i] <- xgboost_TEST$vendite[i] * (1/xgboost_sse) / (1/xgboost_sse + 1/forest_sse + 1/sarima_sse) +
    random_forest_TEST$vendite[i] * (1/forest_sse) / (1/xgboost_sse + 1/forest_sse + 1/sarima_sse) +
    sarima_TEST$vendite[i] * (1/sarima_sse) / (1/xgboost_sse + 1/forest_sse + 1/sarima_sse)
}


summary(test)

# Manually predict outlier areas:
test_final <- merge(test_final, test, by=c("prod", "sottoarea", "data"), all.x=T)
colnames(test_final)[which(colnames(test_final) == 'vendite.x')] <- "vendite"

test_final[which(test_final$sottoarea == 20), ]$predizione <- 0
test_final[which(test_final$sottoarea == 78 && test_final$prod == 2), ]$predizione <- 0
test_final[which(test_final$sottoarea == 32 && test_final$prod == 2), ]$predizione <- 0

mse_final <- (1/nrow(test))*sum((test$predizione - test$vendite)^2)

print(mse_xgboost)
print(mse_sarima)
print(mse_forest)
print(mse_final)


mean(compute_errors(test_final))

# HELPER FUNCTIONS

compute_errors <- function(dataset) {
  results <- data.frame(matrix(NA, ncol=6, nrow=0))
  for (s in unique(dataset$sottoarea)){
    local_test <- filter(dataset, sottoarea==s)
    for (p in unique(local_test$prod)){
      sse <- (1/nrow(filter(local_test, prod==p))*sum((filter(local_test, prod==p)$vendite - filter(local_test, prod==p)$predizione)^2))
      mape <- mean(abs(filter(local_test, prod==p)$vendite - filter(local_test, prod==p)$predizioni)/mean(filter(local_test, prod==p)$vendite))
      maxape <- max(abs(filter(local_test, prod==p)$vendite - filter(local_test, prod==p)$predizioni)/mean(filter(local_test, prod==p)$vendite))

      temp_row <- data.frame(sottoarea=s, prod=p, sse=sse, mape=mape, maxape=maxape)
      results <- rbind(results, temp_row)
    }
  }
  return(results)
}

compute_errors_2 <- function(prediction, test) {
  results <- data.frame(matrix(NA, ncol=6, nrow=0))
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
  return(results)
}





