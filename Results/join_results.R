library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm
library(ranger) # faster version of randomForest
library(nnet) # For binarization
library(zoo) # For time series


# PARAMS
models <- c("random_forest", "sarima", "xgboost")
version <- "FUTURE"
used_error <- 'sse'

# Import predset
dataset <- read.csv("Modified data/dataset_polimi_final_with_holidays_v2.csv", stringsAsFactors=FALSE, row.names=NULL)
dataset <- dataset[,c("zona","area","sottoarea","prod", "data")]
dataset <- unique(dataset)
dataset$data <- as.Date(as.character(dataset$data),format="%Y-%m-%d")

predset <- read.csv("Modified data/predset_complete_with_clusters.csv", stringsAsFactors=FALSE, row.names=NULL)
predset <- predset[,c("zona","area","sottoarea","prod", "data")]
predset <- unique(predset)
predset$data <- as.Date(as.character(predset$data),format="%Y-%m-%d")

predset <- rbind(dataset, predset)

# Turn some features to factors
factorVars <- c("zona","area","sottoarea","prod")

predset[factorVars] <- lapply(predset[factorVars], function(x) as.factor(x))

aree_brutte <- filter(predset, sottoarea == "20")
aree_brutte <- rbind(aree_brutte, filter(predset, sottoarea == "78", prod=="2"))
aree_brutte <- rbind(aree_brutte, filter(predset, sottoarea == "32", prod=="2"))
aree_brutte$vendite <- 0

# remove sottoarea 20
predset <- filter(predset, predset$sottoarea != "20")
# remove sottoarea 78 prodotto 2
temp <- filter(predset, sottoarea == "78", prod=="1")
predset <- filter(predset, sottoarea!="78")
predset <- rbind(predset, temp)
# remove sottoarea 30 prodotto 2
temp <- filter(predset, sottoarea == "32", prod=="1")
predset <- filter(predset, sottoarea != "32")
predset <- rbind(predset, temp)

for(i in seq(1:length(models))){
  
  aree_brutte[paste("vendite_", models[i], sep = "")] <- NA
  aree_brutte[paste(used_error, "_", models[i], sep = "")] <- NA
  
  # Import data
  curr_pred <- read.csv(paste("Results/predizione_", models[i], "_", version, ".csv", sep = ""), stringsAsFactors=FALSE, row.names=NULL)
  curr_pes <- read.csv(paste("Results/pesi_", models[i], ".csv", sep = ""), stringsAsFactors=FALSE, row.names=NULL)
  
  curr_pred <- curr_pred[,c("prod", "sottoarea", "data", "vendite")]
  curr_pred <- unique(curr_pred)
  curr_pred$data <- as.Date(as.character(curr_pred$data),format="%Y-%m-%d")
  curr_pred$vendite <- ifelse(curr_pred$vendite < 0, 0, curr_pred$vendite)
  
  
  # Turn some features to factors
  factorVars <- c("sottoarea", "prod")
  
  curr_pred[factorVars] <- lapply(curr_pred[factorVars], function(x) as.factor(x))
  curr_pes[factorVars] <- lapply(curr_pes[factorVars], function(x) as.factor(x))
  
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

predset <- rbind(predset, aree_brutte)

# Reorder table by key
predset <- predset[order(predset$zona, predset$area, predset$sottoarea, predset$prod, predset$data) , ]
predset["data"] <- lapply(predset["data"], function(x) as.factor(x))

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

write.csv(predset, file=paste("Results/PREDIZIONE_CON_PESI_", version, ".csv", sep = ""), row.names = F)
write.csv(risultati, file=paste("Results/PREDIZIONE_FINALE_", version, ".csv", sep = ""), row.names = F)