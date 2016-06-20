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
predset <- read.csv("Modified data/predset.csv", stringsAsFactors=FALSE)

predset$data <- as.Date(as.character(predset$data),format="%Y-%m-%d")

# Extract information from data field
predset$Giorno_Mese <- day(predset$data)
predset$Giorno_Settimana <- ifelse(wday(predset$data) > 1, wday(predset$data) - 1, 7)
predset$Giorno_Anno <- yday(predset$data)
predset$Mese <- month(predset$data)
predset$Settimana_Anno <- week(predset$data)
predset$Anno <- year(predset$data)
predset$Weekend <- ifelse(predset$Giorno_Settimana > 5, 1, 0)

holiday_list <- as.Date(c("2014-01-01", "2014-01-06", "2014-04-25", "2014-05-01", "2014-06-02",
                          "2014-08-15", "2014-11-01", "2014-12-08", "2014-12-25", "2014-12-26",
                          "2015-01-01", "2015-01-06", "2015-04-25","2015-05-01", "2015-06-02",
                          "2015-08-15", "2015-11-01", "2015-12-08", "2015-12-25", "2015-12-26",
                          "2016-01-01", "2016-01-06", "2016-04-25","2016-05-01", "2016-06-02",
                          "2016-08-15", "2016-11-01", "2016-12-08", "2016-12-25", "2016-12-26",
                          "2014-04-20", "2014-04-21",
                          "2015-04-05", "2015-04-6",
                          "2016-03-27", "2016-03-28"), format = "%Y-%m-%d")

predset$vacanza <- ifelse(predset$data %in% holiday_list, 1, 0)


# Extract seasons from data field
predset$Stagione_Mese <- ifelse(predset$Mese <= 2 | predset$Mese >= 12, 'inverno',
                                       ifelse(3 <= predset$Mese & predset$Mese <= 5, 'primavera',
                                              ifelse(6 <= predset$Mese & predset$Mese <= 8, 'estate',
                                                     ifelse(9 <= predset$Mese & predset$Mese <= 11, 'autunno','?'))))


# Put column names to lower case
names(predset) <- tolower(names(predset))

# Rename "stagione_mese" to "stagione"
names(predset)[names(predset) == 'stagione_mese'] <- 'stagione'

# Rename other columns to something shorter
names(predset)[names(predset) == 'giorno_settimana'] <- 'giorno_sett'

# Rename other columns to something shorter
names(predset)[names(predset) == 'giorno_settimana'] <- 'giorno_sett'

# Create primo_del_mese
predset$primo_del_mese <- ifelse(predset$giorno_mese == 1, 1, 0)


write.csv(predset, file="Modified data/predset_final.csv", row.names = F)

# Import data
predset <- read.csv("Modified data/predset_final.csv", stringsAsFactors=FALSE)
predset_aziendale <- read.csv("Modified data/predizione_aziendale.csv", stringsAsFactors=FALSE)

predset <- merge(predset, predset_aziendale, by = c("data", "prod"))

predset <- predset[order(predset$prod, predset$zona, predset$area, predset$sottoarea, predset$data), ]


write.csv(predset, file="Modified data/predset_complete.csv", row.names = F)
