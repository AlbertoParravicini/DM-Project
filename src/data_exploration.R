library(xts)
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)

setwd("~/DM-Project")
dataset <- read.csv("~/DM-Project/Modified data/dataset_polimi.csv", stringsAsFactors=FALSE, row.names=NULL)

# Remove the x column, if present
dataset <- dataset[ , !(names(dataset) %in% c("X"))]


# Build a smaller datasetset, for testing
# dataset <- dataset[sample(1:nrow(dataset), 1000), ]

# Convert dates to class "Data"
dataset$Data <- as.Date(dataset$Data)

# Remove unneded portions of strings
dataset <- as.data.frame(sapply(dataset, function(x) gsub("Zona_", "", x)))
dataset <- as.data.frame(sapply(dataset, function(x) gsub("Area_", "", x)))
dataset <- as.data.frame(sapply(dataset, function(x) gsub("Sottoarea_", "", x)))
dataset <- as.data.frame(sapply(dataset, function(x) gsub("Prodotto_", "", x)))

# Rename "Categoria_prodotto" to something shorter
names(dataset)[names(dataset) == 'Categoria_prodotto'] <- 'prod'

# Put column names to lower case
names(dataset) <- tolower(names(dataset))

# Rename other columns to something shorter
names(dataset)[names(dataset) == 'giornidellasettimana'] <- 'giorno_sett'
names(dataset)[names(dataset) == 'giornodelmese'] <- 'giorno_mese'
names(dataset)[names(dataset) == 'giornodellanno'] <- 'giorno_anno'

nrow(dataset[dataset$prod==1,])
nrow(dataset[dataset$prod==2,])

summary(dataset)

# Create historical series of product 1.
data_p1 <- filter(dataset, prod == 1, zona == 1) 
data_p1 <- data_p1[, c("data", "vendite")]
data_p1$vendite <- as.numeric(levels(data_p1$vendite))[data_p1$vendite]
data_p1 <- aggregate(cbind(vendite) ~ data , data = data_p1, FUN = sum)

data_p1$data <- as.Date(as.character(data_p1$data),format="%Y-%m-%d")
# create xts object
p1_ts <- xts(data_p1$vendite, data_p1$data)

plot(p1_ts)
abline(lm(p1_ts~time(p1_ts)), lwd=30, col="red")


chart.TimeSeries(p1_ts, legend.loc="bottom", main=" ")
acf(p1_ts, lag.max = 400)
pacf(p1_ts, lag.max = 400)










