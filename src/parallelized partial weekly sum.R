library(doParallel)
registerDoParallel()


i<-0

calculate_sp_sett <- function(i) {
  
  # print(i*100/n)
  myProdotto <- dataset_polimi[i,]$Categoria_prodotto
  myKey <- dataset_polimi[i,]$Key
  myGiorno_Anno <- dataset_polimi[i,]$Giorno_Anno
  mySettimana <- dataset_polimi[i,]$Settimana_Anno
  myAnno <- dataset_polimi[i,]$Anno
  
  temp <- 0
  if (i==1){
    temp <- dataset_polimi[1, ]$Vendite
  }
  else
  {
    if(dataset_polimi[i-1,]$Settimana_Anno != mySettimana){
      temp <- dataset_polimi[i, ]$Vendite
    }
    else {
      temp <- (dataset_polimi[i, ]$Vendite + dataset_polimi[(i-1),]$sp_sett)
    }
  }
  
  return(temp)
}


# result is a vector that must be bind to the original dataframe
sp_sett <- foreach(i=1:n, .inorder = TRUE, .combine = 'c') %dopar% calculate_sp_sett(i)


dataset_polimi <- cbind(dataset_polimi, sp_sett)
