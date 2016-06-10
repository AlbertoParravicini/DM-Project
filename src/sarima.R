library(xts)
library(ggplot2)
library(dplyr)
library(tseries)
library(PerformanceAnalytics)
library(forecast)
library(astsa)
library(Metrics)
library(normwhn.test)
library(nortest)
library(nortestARMA)


# Build a SARIMA model for the specified time series.
# "dataset" is expected to have a column "data" formatted as "%Y-%m-%d" and a numerical column "vendite"
# "num_prod" can be 1 or 2, and it is mandatory.
# "num_zona", "num_area", "num_sottoarea" are optional, but one of them must be specified.
# If more than one is specified, the highest in the hierarchy is considered.

sarima_prediction <- function(data_train, data_test, num_prod = 1, num_zona = 0, num_area = 0, num_sottoarea = 0, details = T) {
  if (nrow(data_train)==0) {
    stop("data_train is empty!")
  }
  if (nrow(data_test) == 0) {
    stop("data_test is empty!")
  }
  
  dataset <- rbind(data_train, data_test)
  
  if (length(dataset$data)==0 | length(dataset$vendite) == 0 || length(dataset$data)==0 || length(dataset$vendite) == 0) {
    stop("the datasets have inappropriate columns!")
  }
  if (!(num_prod %in% unique(dataset$prod))) {
    stop("the product doesn't exist!")
  }
  if (!(num_zona %in% unique(dataset$zona)) && num_zona != 0) {
    stop("the zone doesn't exist!")
  }
  if (!(num_area %in% unique(dataset$area)) && num_area != 0) {
    stop("the area doesn't exist!")
  }
  if (!(num_sottoarea %in% unique(dataset$sottoarea)) && num_sottoarea != 0) {
    stop("the subarea doesn't exist!")
  }
  
  # Filter the dataset depending on the requested prediction!
  if (num_zona != 0) {
    data_p1 <- filter(dataset, prod == num_prod, zona == num_zona)
  }
  else if (num_area != 0) {
    data_p1 <- filter(dataset, prod == num_prod, area == num_area)
  }
  else if (num_sottoarea != 0) {
    data_p1 <- filter(dataset, prod == num_prod, sottoarea == num_sottoarea)
  }
  else {
    stop("Invalid filtering!")
  }
  
  # Length of the training set time series
  train_length <- length(unique(data_train$data))
  # Length of the test set time series
  test_length <- length(unique(data_test$data))
  
  # Create historical series of product 1 in zone 1.
  filtered_data_p1 <- data_p1[, c("data", "vendite")]
  filtered_data_p1$vendite <-filtered_data_p1$vendite
  # When predicting on a zone or area, predict the sum of sales of individual subzones.
  filtered_data_p1 <- aggregate(cbind(vendite) ~ data , data = filtered_data_p1, FUN = sum)
  
  filtered_data_p1$data <- as.Date(as.character(filtered_data_p1$data),format="%Y-%m-%d")
  
  # Create a timeseries object
  ts_full <- zoo(filtered_data_p1$vendite, order.by = filtered_data_p1$data)
  
  # Split the full timeseries in two
  ts_train <- ts_full[1:train_length]
  ts_test <- ts_full[(train_length+1):(test_length+train_length)]
  
  
  # ---------------------------------------------
  # ----- START OF MODELING ---------------------
  # ----- Use a SARIMA(1,1,2,1,1,1,7) ------------
  
  # Try to fit the model by keeping into account the dynamic of the residuals, and predict over the test_set
  fit <- Arima(ts_train, c(2, 0, 2), seasonal = list(order = c(1, 1, 1), period = 7), include.mean = T)
  res <- residuals(fit)
  pred <- forecast(fit, length(ts_test))
  
  if (details) {
    print(fit)
    tsdisplay(res)
  }
  
  # Fit the residuals with a purely seasonal ARMA, of lag 6, and predict over the test_set
  fitres <- Arima(res, c(0, 0, 0), seasonal = list(order = c(1, 1, 1), period = 6, include.mean = T))
  res2 <- residuals(fitres)
  predres <- forecast(fitres, length(ts_test))
  
  if (details) {
    print(fitres)
    tsdisplay(res2)
  }
  
  # Fit the residuals of the residuals with another purely seasonal ARMA, of lag 5, and predict over the test_set
  fitres2 <- Arima(res2, c(0, 0, 0), seasonal = list(order = c(1, 1, 1), period = 5, include.mean = T))
  res3 <- residuals(fitres2)
  predres2 <- forecast(fitres2, length(ts_test))
  
  if (details) {
    print(fitres2)
    tsdisplay(res3)
  }
  
  # Put together the previous predictions
  pred_tot <- pred$mean + predres$mean + predres2$mean
  
  sse <- (1/length(ts_test))*sum((coredata(ts_test) - pred_tot)^2)
  
  # Plot the results
  
  if (details) {
    train_table <- data.frame(vendite=coredata(ts_train), data=index(ts_train), type = "train")
    test_table <- data.frame(vendite=coredata(ts_test), data=index(ts_test), type = "test")
    pred_table <- data.frame(vendite=pred_tot, data=index(ts_test), type = "pred")
    
    table_tot <- rbind(train_table, test_table, pred_table)

    p <- ggplot(table_tot, aes(x=data, y=vendite, color = type)) + 
      coord_cartesian(xlim = c(end(ts_train)-test_length, end(ts_train)+test_length))
    p <- p + geom_line(size = 1) + geom_point(size = 2) + scale_colour_colorblind()
    p <- p + theme_economist() +xlab("Data") + ylab("Numero di vendite") 
    print(p)
    
    # Effective sse of the prediction
    
    if (num_zona != 0) {
      pred_on <- paste("zona ", num_zona)
    }
    else if (num_area != 0) {
      pred_on <- paste("area ", num_area)
    }
    else if (num_sottoarea != 0) {
      pred_on <- paste("sottoarea ", num_sottoarea)
    }
    else {
      stop("Invalid filtering!")
    }
    cat("PREDICTION ON ", pred_on, " OVER ", length(ts_test), " DAYS\n")
    cat("Sum of square errors: ", sse, "\n")
  }

  return(pred_tot)
}
