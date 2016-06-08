library(doParallel)
registerDoParallel()

n<- nrow(dataset_polimi)
i<-0

calculate_sp_mens <- function(i) {

    myProdotto <- dataset_polimi[i,]$Categoria_prodotto
    myKey <- dataset_polimi[i,]$Key
    myGiorno_Anno <- dataset_polimi[i,]$Giorno_Anno
    myMese <- dataset_polimi[i,]$Mese
    myAnno <- dataset_polimi[i,]$Anno
    
    for (j in 1:i){
      if(i-j==0){break} # stop if beginning of dataset reached
      if(dataset_polimi[i-j,]$Categoria_prodotto != myProdotto 
         || dataset_polimi[i-j,]$Key != myKey
         || dataset_polimi[i-j,]$Giorno_Anno > myGiorno_Anno
         || dataset_polimi[i-j,]$Mese != myMese
         || dataset_polimi[i-j,]$Anno != myAnno) {break} # stop if reached a "wrong" j
    }
    
    temp <- 0
    if(j!=0){
      for (k in (i-j+1):i){
        # partial sum
        temp <- temp + dataset_polimi[k, ]$Vendite
      }
    } else {
      temp <- temp + dataset_polimi[0, ]$Vendite
    }
  
  return(temp)
}


# result is a vector that must be bind to the original dataframe
sp_mens <- foreach(i=1:n, .inorder = TRUE, .combine = 'c') %dopar% calculate_sp_mens(i)


dataset_polimi <- cbind(dataset_polimi, sp_mens)
