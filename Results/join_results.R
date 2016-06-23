library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm
library(ranger) # faster version of randomForest
library(nnet) # For binarization
library(zoo) # For time series


# Import predset
predset <- read.csv("Modified data/predset_complete_with_clusters.csv", stringsAsFactors=FALSE, row.names=NULL)
predset <- predset[,c("zona","area","sottoarea","prod", "data")]
predset <- unique(predset)

# PARAMS
models <- c("random_forest", "sarima", "xgboost")
used_error <- 'sse'

for(i in seq(1:length(models))){
  # Import data
  curr_pred <- read.csv(paste("Results/predizione_", models[i], ".csv", sep = ""), stringsAsFactors=FALSE, row.names=NULL)
  curr_pes <- read.csv(paste("Results/pesi_", models[i], ".csv", sep = ""), stringsAsFactors=FALSE, row.names=NULL)
  
  # Rename column vendite
  colnames(curr_pred)[which(colnames(curr_pred) == 'vendite')] <- paste('vendite_', models[i], sep = "")
  
  # Add error
  curr_pes <- curr_pes[,c("sottoarea", "prod", used_error)]
  colnames(curr_pes)[which(colnames(curr_pes) == used_error)] <- paste(used_error, "_", models[i], sep = "")
  
  curr_pred <- merge(curr_pred, curr_pes, by = c("sottoarea", "prod"))
  
  # Merge in predset
  predset <- merge(predset, curr_pred, by = c("sottoarea", "prod", "data"))
}

# Create column vendite
predset$vendite <- 0

# Media pesata delle predizioni
for(i in 1:nrow(predset)) {
  
  numerator <- 0
  denominator <- 0
  
  for(j in seq(1:length(models))) {
    
    curr_weight <- predset[i,][paste(used_error, "_", models[j], sep = "")]
    curr_pred <- predset[i,][paste("vendite_", models[j], sep = "")]
    
    numerator <- numerator + ((1/curr_weight) * curr_pred)
    denominator <- denominator + (1/curr_weight)
    
  }
  
  predset[i,]["vendite"] <- (numerator/denominator)
    
}

# Select columns
risultati <- predset[, c("zona","area","sottoarea","prod", "data", "vendite")]

# Rename attributes
colnames(risultati)[which(colnames(risultati) == 'zona')] <- 'Zona'
colnames(risultati)[which(colnames(risultati) == 'area')] <- 'Area'
colnames(risultati)[which(colnames(risultati) == 'sottoarea')] <- 'Sottoarea'
colnames(risultati)[which(colnames(risultati) == 'prod')] <- 'Categoria_prodotto'
colnames(risultati)[which(colnames(risultati) == 'data')] <- 'Data'
colnames(risultati)[which(colnames(risultati) == 'vendite')] <- 'Vendite'

risultati$Zona <- paste('Zona_', risultati$Zona, sep = "")
risultati$Area <- paste('Area_', risultati$Area, sep = "")
risultati$Sottoarea <- paste('Sottoarea_', risultati$Sottoarea, sep = "")
risultati$Categoria_prodotto <- paste('Prodotto_', risultati$Categoria_prodotto, sep = "")

write.csv(predset, file="Results/PREDIZIONE_PESI.csv", row.names = F)
write.csv(risultati, file="Results/PREDIZIONE.csv", row.names = F)