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
source("src/scoring functions.R")


# Build a SARIMA model for the specified time series.
# "dataset" is expected to have a column "data" formatted as "%Y-%m-%d" and a numerical column "vendite"
# "num_prod" can be 1 or 2, and it is mandatory.
# "num_zona", "num_area", "num_sottoarea" are optional, but one of them must be specified.
# If more than one is specified, the highest in the hierarchy is considered.

setClass(Class = "sarima_pred_res", representation(prediction = "ts", sse = "numeric"))
setClass(Class = "full_sarima_pred_res", representation(predictions = "data.frame", predictions_2 = "data.frame", sse_list = "numeric"))

sarima_prediction <- function(data_train, data_test = NA, prediction_length = 0, num_prod = 1, num_zona = 0, num_area = 0, num_sottoarea = 0, details = T, method = "CSS-ML", regressors = NULL) {
  if (nrow(data_train)==0) {
    stop("data_train is empty!")
  }
  if (all(is.na(data_test)) && prediction_length == 0) {
    stop("prediction length = 0 and data_test is empty!")
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
    filtered_data <- filter(dataset, prod == num_prod, zona == num_zona)
  }
  else if (num_area != 0) {
    filtered_data <- filter(dataset, prod == num_prod, area == num_area)
  }
  else if (num_sottoarea != 0) {
    filtered_data <- filter(dataset, prod == num_prod, sottoarea == num_sottoarea)
  }
  else {
    stop("Invalid filtering!")
  }
  data_train$data <- as.Date(as.character(data_train$data),format="%Y-%m-%d")
  if (all(!is.na(data_test))) {
    data_test$data <- as.Date(as.character(data_test$data),format="%Y-%m-%d")
  }
  
    
  # Length of the training set time series
  train_length <- length(unique(data_train$data))
  # Length of the test set time series
  
  if (all(!is.na(data_test))) {
    prediction_length <- length(unique(data_test$data))
  }
  
  # When predicting on a zone or area, predict the sum of sales of individual subzones.
  filtered_data <- aggregate(cbind(vendite, vendite_giorn_prod) ~ data , data = filtered_data, FUN = sum)
  filtered_data$data <- as.Date(as.character(filtered_data$data),format="%Y-%m-%d")
  
  # Create a timeseries object
  ts_full <- zoo(filtered_data$vendite, order.by = filtered_data$data)
  
  
  # Split the full timeseries in two
  ts_train <- ts_full[1:train_length]
  ts_test <- ts_full[(train_length+1):(prediction_length+train_length)]
  
  # Use external inputs as regressors.
  #train_regressors <- matrix(c(filtered_data$sp_settimana[1:train_length], filtered_data$sp_mese[1:train_length], filtered_data$sp_anno[1:train_length]), ncol = 3)
  #test_regressors <- matrix(c(filtered_data$sp_settimana[(train_length+1):(prediction_length+train_length)], filtered_data$sp_mese[(train_length+1):(prediction_length+train_length)], filtered_data$sp_anno[(train_length+1):(prediction_length+train_length)]), ncol = 3)
  train_regressors <- matrix(filtered_data$vendite_giorn_prod[1:train_length], ncol = 1)
  
  # Need to predict the test regressors!
  if (all(is.na(regressors))) {
    test_regressors <- pred_test_regressors(end(ts_train)+1, prediction_length = prediction_length, method = method, num_prod = num_prod)$mean
    print(test_regressors)
  } else {
    test_regressors <- regressors$mean
  }

  
  # ---------------------------------------------
  # ----- START OF MODELING ---------------------
  # ----- Use a SARIMA(1,1,2,1,1,1,7) ------------
  
  # Try to fit the model by keeping into account the dynamic of the residuals, and predict over the test_set
  fit <- Arima(ts_train, c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 7), include.mean = T, method = method, xreg = train_regressors)
  res <- residuals(fit)
  pred <- forecast(fit, prediction_length, xreg = test_regressors)
  
  
  if (details) {
    print(fit)
    tsdisplay(res)
  }
  
  # Fit the residuals with a purely seasonal ARMA, of lag 6, and predict over the test_set
  fitres <- Arima(res, c(0, 0, 0), seasonal = list(order = c(1, 1, 1), period = 6), include.mean = T, method = method, xreg = train_regressors)
  res2 <- residuals(fitres)
  predres <- forecast(fitres, prediction_length, xreg = test_regressors)
  
  
  if (details) {
    print(fitres)
    tsdisplay(res2)
  }
  
  # Fit the residuals of the residuals with another purely seasonal ARMA, of lag 5, and predict over the test_set
  fitres2 <- Arima(res2, c(0, 0, 0), seasonal = list(order = c(1, 1, 0), period = 5), include.mean = T, method = method, xreg = train_regressors)
  res3 <- residuals(fitres2)
  predres2 <- forecast(fitres2, prediction_length, xreg = test_regressors)
  
  
  
  if (details) {
    print(fitres2)
    tsdisplay(res3)
  }
  
  # Put together the previous predictions
  pred_tot <- pred$mean + predres$mean + predres2$mean
  
  # If negative values are predicted, round them to zero.
  pred_tot <- ifelse(pred_tot < 0, 0, pred_tot)
  
  sse <- -1
  if (all(!is.na(data_test))) {
    sse <- (1/prediction_length)*sum((coredata(ts_test) - pred_tot)^2)
  }

    
  
  # Plot the results
  
  if (details) {
    train_table <- data.frame(vendite=coredata(ts_train), data=index(ts_train), type = "train")
    test_table <- data.frame(vendite=coredata(ts_test), data=seq.Date(from=max(data_train$data)+1, length.out = prediction_length, by = 1), type = "test")
    pred_table <- data.frame(vendite=pred_tot, data=seq.Date(from=max(data_train$data)+1, length.out = prediction_length, by = 1), type = "pred")
    
    table_tot <- rbind(train_table, test_table, pred_table)

    p <- ggplot(table_tot, aes(x=data, y=vendite, color = type)) + 
      coord_cartesian(xlim = c(end(ts_train)-prediction_length, end(ts_train)+prediction_length))
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
    if (all(!is.na(data_test))) {
      cat("Sum of square errors: ", sse, "\n")
    }
  }

  return(new("sarima_pred_res", prediction=pred_tot, sse=sse))
}

full_sarima_prediction <- function(train, test = NA, prediction_length = 0, details = F, ...) {
  
  sse_list <- c()
  result_list <- data.frame(matrix(NA, nrow = 0, ncol = 12))
  result_list_2 <- data.frame(matrix(NA, nrow = 0, ncol = 4))
  if (all(!is.na(data_test))) {
    prediction_length <- length(unique(data_test$data))
  }
  
  
  for (prod_i in 1:2) {
    cat("prodotto: ", prod_i, "\n")
    exo_input <- pred_test_regressors(max(train$data)+1, prediction_length = prediction_length, num_prod = prod_i, ...)
    for (sottoarea_i in sort(unique(train$sottoarea))[1:2]) {
      cat("sottoarea: ", sottoarea_i, "\n")
      if (all(!is.na(test))) {
        res_temp <- sarima_prediction(train, test, num_prod = prod_i, num_sottoarea = sottoarea_i, details = details, regressors = exo_input, ...)
      }
      else {
        res_temp <- sarima_prediction(train, prediction_length = prediction_length, num_prod = prod_i, num_sottoarea = sottoarea_i, details = details, regressors = exo_input, ...)
      }
      sse_list <- c(sse_list, attr(res_temp, "sse"))
      cat("\nNUMERO PRODOTTO: ", prod_i, "\n")
      cat("NUMERO SOTTOAREA: ", sottoarea_i, "\n")
      cat("PREDIZIONI: \n")
      cat(coredata(attr(res_temp, "prediction")), "\n")
      temp_row <- data.frame(prod = as.numeric(prod_i), sottoarea = as.numeric(sottoarea_i), matrix(as.numeric(coredata(attr(res_temp, "prediction"))), ncol=length(coredata(attr(res_temp, "prediction")))))
      result_list <- rbind(result_list, temp_row)

      cat("SSE: ", attr(res_temp, "sse"), "\n")
      # Build a second result data frame, built as (prod - area - date - sales).
      result_list_2_temp <- cbind(prod = as.numeric(prod_i), sottoarea = as.numeric(sottoarea_i), data = as.numeric(index(attr(res_temp, "prediction"))), vendite = as.numeric(coredata(attr(res_temp, "prediction"))))
      result_list_2 <- rbind(result_list_2, result_list_2_temp)
    }
  }
  result_list_2$data <- as.Date("1970-01-01", format="%Y-%m-%d") + result_list_2$data
  cat("\nSSE MEDIO: ", mean(sse_list), "\n")
  
  return(new("full_sarima_pred_res", predictions = result_list, predictions_2 = result_list_2, sse_list = sse_list))
}

# result_list_2[, c("prod", "sottoarea")] <- lapply(result_list_2[, c("prod", "sottoarea")], function(x) as.factor(x))

evaluate_sarima_results <- function(validation, prediction) {
  # Joim the datasets based on date and subarea
  common_dates <- intersect(unique(validation$data), unique(prediction$data))
  common_subareas <- intersect(unique(validation$sottoarea), unique(prediction$sottoarea))
  
  validation <- filter(validation, data %in% common_dates, sottoarea %in% common_subareas)
  prediction <- filter(prediction, data %in% common_dates, sottoarea %in% common_subareas)

  validation <- validation[order(validation$prod, validation$sottoarea, validation$data), ]
  prediction <- prediction[order(prediction$prod, prediction$sottoarea, prediction$data), ]
  
  mse = mse(validation$vendite, prediction$vendite)
  ape_list = ape(validation, prediction)
  mean_ape = meanape(validation, prediction)
  max_ape = maxape(validation, prediction)
  
  return(data.frame(mse = mse, mape = mean_ape, max_ape = max_ape))
}

pred_test_regressors <- function(prediction_start, prediction_length, method = "ML-CSS", num_prod = 1) {
  setwd("~/DM-Project")

  # Use the exogen signal of the overall sales
  vendite_giornaliere_prod <- read.csv("~/DM-Project/Modified data/vendite_giornaliere_prod.csv", row.names=NULL, stringsAsFactors=FALSE)
  vendite_giornaliere_prod$prod <- as.factor(vendite_giornaliere_prod$prod)
  
  # Turn dates to "Date" class
  prediction_start <- as.Date(prediction_start, format = "%Y-%m-%d")
  vendite_giornaliere_prod$data <- as.Date(as.character(vendite_giornaliere_prod$data),format="%Y-%m-%d")
  
  colnames(vendite_giornaliere_prod)[which(colnames(vendite_giornaliere_prod) == 'vendite_giorn_prod')] <- 'vendite'
  
  summary(vendite_giornaliere_prod)
  
  if (!prediction_start %in% vendite_giornaliere_prod$data) {
    stop("Invalid starting date")
  }

  vendite_giornaliere_prod <- filter(vendite_giornaliere_prod, prod == num_prod)
  # Use only the data until "prediction_start"
  test_vendite <- filter(vendite_giornaliere_prod, data >= prediction_start)
  vendite_giornaliere_prod <- filter(vendite_giornaliere_prod, data < prediction_start)

  
  ts_vendite <- zoo(vendite_giornaliere_prod$vendite, order.by = vendite_giornaliere_prod$data)
  tsdisplay(diff(ts_vendite,7))
  
  # Try to fit the model by keeping into account the dynamic of the residuals, and predict over the test_set
  # 1 0 2 3 2 5
  fit <- Arima(ts_vendite, c(1, 0, 2), seasonal = list(order = c(2, 2, 5), period = 7), include.mean = T)#, method = method)
  res <- residuals(fit)
  tsdisplay(res, lag.max = 60)
  print(fit)
  pred <- forecast(fit, prediction_length)
  print((1/prediction_length)*sum((test_vendite$vendite - pred$mean)^2))

  
  # # Fit the residuals with a purely seasonal ARMA, of lag 6, and predict over the test_set
  # fitres <- Arima(res, c(0, 0, 0), seasonal = list(order = c(1, 1, 1), period = 6), include.mean = T)#, method = method)
  # res2 <- residuals(fitres)
  # predres <- forecast(fitres, prediction_length)
  # 
  # 
  # # Fit the residuals of the residuals with another purely seasonal ARMA, of lag 5, and predict over the test_set
  # fitres2 <- Arima(res2, c(0, 0, 0), seasonal = list(order = c(1, 1, 0), period = 5), include.mean = T)#, method = method)
  # res3 <- residuals(fitres2)
  # predres2 <- forecast(fitres2, prediction_length)
  # 
  # tsdisplay(res3, main ="overall prediction - res 3")
  # 
  # # Put together the previous predictions
  # pred_tot <- pred$mean + predres$mean + predres2$mean
  # 
  # # If negative values are predicted, round them to zero.
  # pred_tot <- ifelse(pred_tot < 0, 0, pred_tot)
  # 
  # print((1/prediction_length)*sum((test_vendite$vendite - pred_tot)^2))

  return(coredata(pred))
}
