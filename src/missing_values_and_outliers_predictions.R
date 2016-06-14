library(dplyr)
library(ranger)
library(ggplot2)
library(ggthemes)
library(zoo)
library(xgboost)

#dataset_polimi <- read.csv("~/DM-Project/Modified data/dataset_polimi_final.csv", stringsAsFactors=FALSE, row.names=NULL)

dataset_polimi <- arrange(dataset_polimi_final_vendite_giorn, X)


# Convert dates to class "Data"
dataset_polimi$data <- as.Date(dataset_polimi$data)
# Convert "vendite" to numeric values if needed
if (class(dataset_polimi$vendite) == "factor") {
  dataset_polimi$vendite <- as.numeric(levels(dataset_polimi$vendite))[dataset_polimi$vendite]
}

# Turn some features to factors
factorVars <- c('zona','area', "sottoarea",
                'prod','giorno_mese', "giorno_settimana", "giorno_anno", "mese", "settimana_anno", "anno", "weekend","stagione", "key", "primo_del_mese")

dataset_polimi[factorVars] <- lapply(dataset_polimi[factorVars], function(x) as.factor(x))

# build a simple single random forest on the dataset where no missing values / outliers are

rfs_train <- filter(dataset_polimi, azienda_chiusa==0 & data != "2014-06-30")

n<- 200

rfs_model <- ranger(rfs_train,
                      formula=vendite ~ key+ zona + area + sottoarea  + prod + giorno_settimana + giorno_mese + giorno_anno + settimana_anno + mese + anno + weekend + stagione + primo_del_mese,
                      num.trees = n, importance="impurity", write.forest = T)

# handle missing values for 1/1/2014 and for 19-25/3/2014 and outliers for 30/6/2014: substitute with a prediction value


for(i in 1:nrow(dataset_polimi)){
  if(dataset_polimi[i,]$azienda_chiusa == 1 || dataset_polimi[i,]$data == "2014-06-30"){
    dataset_polimi[i,]$vendite <- predict(rfs_model, dataset_polimi[i,])$predictions
  }
}

dataset_polimi <- dataset_polimi[ , !(names(dataset_polimi) %in% c("X"))]

write.csv(dataset_polimi, file="Modified data/dataset_polimi_predictions.csv", row.names = FALSE)
