library(xts)
library(ggplot2)
library(dplyr)
library(tseries)
library(PerformanceAnalytics)
library(forecast)
library(astsa)
library(Metrics)
library(ggthemes) # visualization

prediction_length = 10



# DATA IMPORT AND CLEANING
# ------------------------------------------------------
# ------------------------------------------------------

setwd("~/DM-Project")
dataset <- read.csv("~/DM-Project/Modified data/dataset_polimi_clusterized_tot_pred.csv", stringsAsFactors=FALSE, row.names=NULL)
# Remove the x column, if present
dataset <- dataset[ , !(names(dataset) %in% c("X"))]


# Convert dates to class "Data"
dataset$data <- as.Date(dataset$data)
# Convert "vendite" to numeric values if needed
if (class(dataset$vendite) == "factor") {
  dataset$vendite <- as.numeric(levels(dataset$vendite))[dataset$vendite]
}

# Turn some features to factors
factorVars <- c('zona','area', "sottoarea",
                'prod','giorno_mese', "giorno_settimana", "giorno_anno", "mese", "settimana_anno", "anno", "weekend","stagione", "key", "azienda_chiusa", "primo_del_mese", "cluster3", "cluster6", "cluster20")

dataset[factorVars] <- lapply(dataset[factorVars], function(x) as.factor(x))

summary(dataset)

holiday_list <- as.Date(c("2014-01-01", "2014-01-06", "2014-04-25", "2014-05-01", "2014-06-02",
                  "2014-08-15", "2014-11-01", "2014-12-08", "2014-12-25", "2014-12-26",
                  "2015-01-01", "2015-01-06", "2015-04-25","2015-05-01", "2015-06-02",
                  "2015-08-15", "2015-11-01", "2015-12-08", "2015-12-25", "2015-12-26",
                  "2016-01-01", "2016-01-06", "2016-04-25","2016-05-01", "2016-06-02",
                  "2016-08-15", "2016-11-01", "2016-12-08", "2016-12-25", "2016-12-26",
                  "2014-04-20", "2014-04-21",
                  "2015-04-05", "2015-04-6",
                  "2016-03-27", "2016-03-28"), format = "%Y-%m-%d")

dataset$vacanza <- ifelse(dataset$data %in% holiday_list, 1, 0)


write.csv(dataset, file="Modified data/dataset_polimi_with_holidays.csv", row.names = FALSE)