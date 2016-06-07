library(xts)
library(ggplot2)
library(dplyr)
library(tseries)
library(PerformanceAnalytics)
library(forecast)


# DATA IMPORT AND CLEANING
# ------------------------------------------------------
# ------------------------------------------------------

setwd("~/DM-Project")
dataset <- read.csv("~/DM-Project/Modified data/dataset_polimi.csv", stringsAsFactors=FALSE, row.names=NULL)
# Remove the x column, if present
dataset <- dataset[ , !(names(dataset) %in% c("X"))]

# Build a smaller datasetset, for testing
# dataset <- dataset[sample(1:nrow(dataset), 1000), ]

# Convert dates to class "Data"
dataset$data <- as.Date(dataset$data)
# Convert "vendite" to numeric values if needed
if (class(dataset$vendite) == "factor") {
  dataset$vendite <- as.numeric(levels(dataset$vendite))[dataset$vendite]
}

# Turn some features to factors
factorVars <- c('zona','area', "sottoarea",
                'prod','giorno_mese', "giorno_settimana", "giorno_anno", "mese", "settimana_anno", "anno", "weekend","stagione", "key")

dataset[factorVars] <- lapply(dataset[factorVars], function(x) as.factor(x))

summary(dataset)

# ------------------------------------------------------
# ------------------------------------------------------


# Create historical series of product 1 in zone 1.
data_p1 <- filter(dataset, prod == 1, zona == 1) 
data_p1 <- data_p1[, c("data", "vendite")]
data_p1$vendite <-data_p1$vendite
data_p1 <- aggregate(cbind(vendite) ~ data , data = data_p1, FUN = sum)

data_p1$data <- as.Date(as.character(data_p1$data),format="%Y-%m-%d")

# create a timeseries object
p1_ts <- zoo(data_p1$vendite, order.by = data_p1$data)

# Quickly inspect the series
plot(p1_ts)
abline(reg<-lm(p1_ts~time(p1_ts)), lwd=3, col="green")
# It has a mean != 0 and also shows a slight linear trend
plot(residuals(reg))
# Remove the polarization
abline(lm(residuals(reg)~time(residuals(reg))), lwd=3, col="green")
mean(residuals(reg))

dep_p1 <- residuals(reg)
acf(dep_p1)

# There is a clear weekly seasonality!
# Let's de-seasonalize (is this even a word?) the data
des_p1 <- diff(dep_p1, 7)
acf(des_p1, lag.max = 200)

# There are still some minor seasonalities!
# We ignore the for now, the process is more or less stationary
pp.test(des_p1)

# plot acf and pcf of both dep_p1 and des_p1
tsdisplay(dep_p1)
tsdisplay(des_p1)

# What is th order of the ARIMA? 
# The weekly seasonality can be modeled as part of the ARIMA: (1 + a*z^-1)(1 + b*z^-7)*y(t) = w(t)
# It is also possible to operate directly on the non depolarized signal!
fit <- Arima(p1_ts, c(1, 0, 1), seasonal = list(order = c(1, 0, 1), period = 7), include.mean = T)

pred <- forecast(fit, 50)
plot(pred)
lin <- reg$coefficients[1] + reg$coefficients[2]*seq(from = start(pred$mean)[1], length.out = 50)
tot_pred <- pred$mean + lin

# plot the results
plot(p1_ts, type="l")
lines(tot_pred, col="red")



