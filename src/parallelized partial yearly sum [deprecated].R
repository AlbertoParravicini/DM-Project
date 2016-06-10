library(doParallel)
registerDoParallel()

n<-nrow(dataset_polimi)
# n<-365
i<-0

calculate_sp_anno <- function(i) {

    myProdotto <- dataset_polimi[i,]$Categoria_prodotto
    # myKey <- dataset_polimi[i,]$Key
    # myGiorno_Anno <- dataset_polimi[i,]$Giorno_Anno
    myAnno <- dataset_polimi[i,]$Anno
    
    for (j in 0:i){
      if(i-j==0){break} # stop if beginning of dataset reached
      if(dataset_polimi[i-j,]$Categoria_prodotto != myProdotto 

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
sp_anno <- foreach(i=1:n, .inorder = TRUE, .combine = 'c') %dopar% calculate_sp_anno(i)
# print()

dataset_polimi <- cbind(dataset_polimi, sp_anno)
