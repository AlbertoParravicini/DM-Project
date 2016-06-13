# mean absolute percentage error (MAPE)
meanape <- function(real, forecast){
  return(mean(abs((real - forecast)/real)))
}
# max absolute percentage errore
maxape <- function(real, forecast){
  return(max(abs((real - forecast)/real)))
}

