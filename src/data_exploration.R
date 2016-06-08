library(xts)
library(ggplot2)
library(dplyr)
library(tseries)
library(PerformanceAnalytics)
library(forecast)
library(astsa)
library(Metrics)


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

# Plot sales for each day of the week
daily <- aggregate(cbind(vendite) ~ giorno_settimana , data = dataset, FUN = sum)
barplot(daily$vendite)
# Can we do better?
p0 <- ggplot(dataset, aes(giorno_settimana, vendite)) + geom_boxplot()
p0
# Sales on monday seems to be lower than the other days, but there are many high outliers.
# Remove the outliers
# compute lower and upper whiskers
ylim1 = boxplot.stats(dataset$vendite)$stats[c(1, 5)]
# scale y limits based on ylim1
p1 <- p0 + coord_cartesian(ylim = ylim1*1.05)
p1
# As expected, not many sales in weekends. The sales during the week are more or less constant

# Plot sales for each month
daily <- aggregate(cbind(vendite) ~ mese , data = dataset, FUN = sum)
barplot(daily$vendite)
# Can we do better?
p0 <- ggplot(dataset, aes(mese, vendite)) + geom_boxplot()
p0
# TODO: Comment
# Remove the outliers
# compute lower and upper whiskers
ylim1 = boxplot.stats(dataset$vendite)$stats[c(1, 5)]
# scale y limits based on ylim1
p1 <- p0 + coord_cartesian(ylim = ylim1*1.05)
p1
# TODO: Comment


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

# What is the order of the ARIMA? 
# The weekly seasonality can be modeled as part of the ARIMA: (1 + a*z^-1)(1 + b*z^-7)*y(t) = w(t)
# It is also possible to operate directly on the non depolarized signal!
# Using (1,1,2,1,1,1) gives worse results, but a better prediction on the validation set below: 
# this should be checked
fit <- Arima(p1_ts, c(1, 0, 2), seasonal = list(order = c(1, 1, 1), period = 7), include.mean = T)

pred <- forecast(fit, 50)
plot(pred)

fit

# Can we still fit something over the residuals?
res <- residuals(fit)
tsdisplay(residuals(fit))

auto.arima(res, stepwise = F)

# Probably not, even though there is still a small spike on 7

# # Trying out the astsa package
# 
# acf2(p1_ts, 100)
# acf2(diff(p1_ts, 1), 100)
# 
# fit <- sarima(p1_ts, 1,0,2,1,1,1,7)
# pred <- sarima.for(p1_ts, 50, 1,1,2,1,1,1,7)
# 
# # Not bad at all!

# How good is this model? let's use a validation set!

p1_train <- p1_ts[1:(0.9*length(p1_ts))]
p1_test <- p1_ts[(length(p1_train)+ 1):length(p1_ts)]

fit <- Arima(p1_train, c(1, 0, 2), seasonal = list(order = c(1, 1, 1), period = 7), include.mean = T, include.drift = T)

pred <- forecast(fit, length(p1_test))
plot(pred)
lines(p1_test, col="red")

# Effective sse of the prediction
(1/length(p1_test))*sum((coredata(p1_test) - pred$mean)^2)

# Let's try again with the requested 10 days prediction

p1_train <- p1_ts[1:(length(p1_ts)-10)]
p1_test <- p1_ts[length(p1_train)+ 1:10]

# Use sarima as it easier to plot, the model is pretty much the same as Arima
fit <- sarima(p1_train, 1,0,2,1,1,1,7, details = F)
pred <- sarima.for(p1_train, length(p1_test), 1,0,2,1,1,1,7)
lines(p1_test, col="green")
points(p1_test, col="green")

# Effective sse of the prediction
(1/length(p1_test))*sum((coredata(p1_test) - pred$pred)^2)


