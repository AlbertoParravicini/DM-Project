library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm
library(ranger) # faster version of randomForest
library(nnet) # For binarization
library(zoo) # For time series

# Import data
dataset <- read.csv("Modified data/dataset_polimi_final_with_holidays_v2.csv", stringsAsFactors=FALSE)
predset <- read.csv("Modified data/predset_complete.csv", stringsAsFactors=FALSE)

clusters <- select(dataset, sottoarea, cluster3, cluster6, cluster20)
clusters <- unique(clusters)

predset <- merge(predset, clusters, by = c("sottoarea"))

write.csv(predset, file="Modified data/predset_complete_with_clusters.csv", row.names = F)
