library(dplyr)

# mean squared errors (MSE)
mse <- function(real, forecast){
  return((1/length(real))*sum((real - forecast)^2))
}


ape <- function(real_ds, forecast){
  temp <- cbind(real_ds, forecast$vendite)
  sottoarea <- c()
  valore <- c()
  # per ogni sottoarea
  for (s in unique(temp$sottoarea)){
    temp_s <- filter(temp, sottoarea==s)
    sottoarea <- c(sottoarea, s)
    valore <- c(valore, mean(abs(temp_s$vendite - temp_s$forecast)/mean(temp_s$vendite)))
  }
  valore = replace(valore, is.na(valore), 0)
  return(data.frame(sottoarea, valore))
}

meanape <- function(real_ds, forecast){
  return(mean(ape(real_ds, forecast)$valore))
}

maxape <- function(real_ds, forecast){
  return(max(ape(real_ds,forecast)$valore))
}