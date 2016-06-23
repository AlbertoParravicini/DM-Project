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
library(normwhn.test)
library(nortest)
library(nortestARMA)

prediction_length = 10
setwd("~/DM-Project")


# DATA IMPORT AND CLEANING
# ------------------------------------------------------
# ------------------------------------------------------


dataset <- read.csv("Modified data/dataset_polimi_final_with_holidays_v2.csv", stringsAsFactors=FALSE, row.names=NULL)
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
                'prod','giorno_mese', "giorno_settimana", "giorno_anno", "vendite_missing", "mese", "settimana_anno", "anno", "weekend","stagione", "key", "primo_del_mese", "cluster3", "cluster6", "cluster20", "vacanza")

dataset[factorVars] <- lapply(dataset[factorVars], function(x) as.factor(x))

dataset <- dataset[order(dataset$prod, dataset$zona, dataset$area, dataset$sottoarea), ]

#dataset <- dataset[ , !(names(dataset) %in% c("vendite_giorn_prod"))]
#colnames(dataset)[which(colnames(dataset) == 'vendite_giorn_prod.x')] <- 'vendite_giorn_prod'

summary(dataset)

# Build a smaller dataset, for testing
s_area = sample(unique(dataset$sottoarea), 1)
data_p1 <- filter(dataset, prod == 1, sottoarea == 1)



# ------------------------------------------------------
# START OF MODELLING
# ------------------------------------------------------

# Create historical series of product 1 in zone 1.
filtered_data_p1 <- data_p1[, c("data", "vendite")]
filtered_data_p1$vendite <-filtered_data_p1$vendite
filtered_data_p1 <- aggregate(cbind(vendite) ~ data , data = filtered_data_p1, FUN = mean)

filtered_data_p1$data <- as.Date(as.character(filtered_data_p1$data),format="%Y-%m-%d")

# create a timeseries object
p1_ts <- zoo(filtered_data_p1$vendite, order.by = filtered_data_p1$data)

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
tsdisplay(diff(res,6))

# Try to fit the residuals with a purely seasonal ARMA
fitres <- Arima(res, c(0, 0, 0), seasonal = list(order = c(1, 1, 1), period = 6, include.mean = T))
tsdisplay(residuals(fitres))

# Fit the residuals of the residuals  with a 5-delay seasonal arma
fitres2 <- Arima(residuals(fitres), c(0, 0, 0), seasonal = list(order = c(1, 1, 1), period = 5, include.mean = T))
tsdisplay(residuals(fitres2))





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

p1_train <- p1_ts[1:(length(p1_ts)-prediction_length)]
p1_test <- p1_ts[length(p1_train)+ 1:prediction_length]

# Use sarima as it easier to plot, the model is pretty much the same as Arima
fit <- sarima(p1_train, 1,0,2,1,1,1,7, details = F)
pred <- sarima.for(p1_train, length(p1_test), 1,0,2,1,1,1,7)
lines(p1_test, col="green")
points(p1_test, col="green")

# Effective sse of the prediction
(1/length(p1_test))*sum((coredata(p1_test) - pred$pred)^2)

# PART 2: modelling the dynamic of the residuals
# Use a SARIMA(1,1,2,1,1,1,7)

# Try to fit the model by keeping into account the dynamic of the residuals
fit <- Arima(p1_train, c(1, 1, 2), seasonal = list(order = c(1, 1, 1), period = 7), include.mean = T)
fit
res <- residuals(fit)
tsdisplay(res)
pred <- forecast(fit, length(p1_test))
# Fit the residuals with a purely seasonal ARMA, of lag 6
fitres <- Arima(res, c(0, 0, 0), seasonal = list(order = c(1, 1, 1), period = 6, include.mean = T))
fitres
res2 <- residuals(fitres)
tsdisplay(res2)
predres <- forecast(fitres, length(p1_test))
# Fit the residuals of the residuals with another purely seasonal ARMA, of lag 5
fitres2 <- Arima(res2, c(0, 0, 0), seasonal = list(order = c(1, 1, 1), period = 5, include.mean = T))
fitres2
res3 <- residuals(fitres2)
tsdisplay(res3, lag.max = 600)
predres2 <- forecast(fitres2, length(p1_test))

# Put together the previous predictions
pred_tot <- pred$mean + predres$mean + predres2$mean

plot(p1_train[(length(p1_train)-prediction_length):length(p1_train)], type="l", xlim=c(end(p1_train)-50, end(p1_train)+50))
lines(p1_test, col="green")
lines(pred_tot, col="red")
points(p1_test, col="green")
points(pred_tot, col="red")

# Effective sse of the prediction
(1/length(p1_test))*sum((coredata(p1_test) - pred_tot)^2)

# ---------------------------------------
# Use Random forest
# ---------------------------------------
data_p1$data <- as.Date(as.character(data_p1$data),format="%Y-%m-%d")

data_train <- data_p1[1:(nrow(data_p1)-prediction_length), ]
data_test <- data_p1[nrow(data_train) +1:prediction_length, ]
forest_reg <- ranger(data_train,
                     formula=vendite ~ giorno_settimana + giorno_mese + giorno_anno + mese + weekend + stagione + primo_del_mese + azienda_chiusa + longitudine + latitudine + cluster3 + cluster6 + cluster20,
                     num.trees = 400, importance="impurity", write.forest = T)

#Get importance of forest_reg
importance_ranger <- importance(forest_reg)

# Use ggplot2 to visualize the relative importance of variables

importance_df <- data.frame(name = names(importance_ranger), importance_ranger)
ggplot(importance_df, aes(x = reorder(name, importance_ranger), 
                          y = importance_ranger)) +
  geom_bar(stat='identity', colour = 'black') +
  labs(x = 'Variables', title = 'Relative Variable Importance') +
  coord_flip() + 
  theme_few()

forest_pred <- predict(forest_reg, data_test)

plot(data_train$vendite[(nrow(data_train)-prediction_length):nrow(data_train)], type="l", xlim=c(0,100))

plot.ts(as.ts(forest_pred$predictions, order.by = index(forest_pred$predictions)), col="red", ylim=c(0, 12))
lines(as.ts(data_test$vendite, order.by = index(data_test$vendite)), col="green")
points(data_test$vendite, col="green")
points(forest_pred$predictions, col="red")
lines(coredata(pred_tot), col="blue")
points(coredata(pred_tot), col="blue")

# Effective sse of the prediction
(1/length(p1_test))*sum((coredata(p1_test) - pred_tot)^2)

(1/nrow(data_test))*sum((forest_pred$predictions - data_test$vendite)^2)


# ---------------------------------------
# Use XGBoost
# ---------------------------------------
xg_train <- xgb.DMatrix(model.matrix(~giorno_settimana + giorno_mese + giorno_anno + mese + weekend + stagione, data=data_train),
            label=data_train$vendite, missing=NA)
xg_test <- xgb.DMatrix(model.matrix(~giorno_settimana + giorno_mese + giorno_anno + mese + weekend + stagione, data=data_test),
                        label=data_test$vendite, missing=NA)

xgb_model <- xgboost(xg_train, data_train$vendite, nrounds = 45)
xgb_pred <- predict(xgb_model, xg_test)

plot(data_train$vendite[(nrow(data_train)-prediction_length):nrow(data_train)], type="l", xlim=c(0,100))

plot.ts(as.ts(xgb_pred, order.by = index(xgb_pred)), col="red", ylim=c(0, 12))
lines(as.ts(data_test$vendite, order.by = index(data_test$vendite)), col="black")
points(data_test$vendite, col="black")
points(xgb_pred, col="red")
lines(coredata(pred_tot), col="blue")
points(coredata(pred_tot), col="blue")
lines(as.ts(forest_pred$prediction, order.by = index(forest_pred$prediction)), col="green")
points(forest_pred$predictions, col="green")

# Effective sse of the prediction
cat("sottoarea: ", s_area, "\n")
(1/length(p1_test))*sum((coredata(p1_test) - pred_tot)^2)
(1/nrow(data_test))*sum((forest_pred$predictions - data_test$vendite)^2)
(1/nrow(data_test))*sum((xgb_pred - data_test$vendite)^2)



train <- filter(dataset, data <= max(data) - prediction_length)
test <- filter(dataset, data > max(data) - prediction_length)


