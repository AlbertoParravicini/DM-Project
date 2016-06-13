# mean absolute prediction error (MAPE)
meanape <- function(real, forecast){
  return(mean(abs(real - forecast)))
}
# max absolute prediction error
maxape <- function(real, forecast){
  return(max(abs(real - forecast)))
}
# mean squared errors (MSE)
mse <- function(real, forecast){
  return((1/length(real))*sum((real - forecast)^2))
}