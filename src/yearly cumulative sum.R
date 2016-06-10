library(dplyr)

dataset <- read.csv("~/DM-Project/dataset_polimi.csv", stringsAsFactors=FALSE, row.names=NULL)
# need to have run "data preparation.R" on this dataset!


v_sp_anno <- vector(mode="numeric", length=0L)

for (k in unique(dataset$Key)){
  for (y in unique(dataset$Anno)){
    v_temp <- vector(mode="numeric", length=0L)
    print(c(k, y))
    temp <- filter(dataset, Anno==y & Key==k)$Vendite
    if(length(temp)!=0){
     v_temp <- cumsum(temp) 
     v_sp_anno <- c(v_sp_anno, v_temp)
    }
  }
}

dataset <- cbind(dataset, v_sp_anno)
names(dataset)[names(dataset) == 'v_sp_anno'] <- 'sp_anno'


write.csv(dataset, file="Modified data/dataset_polimi_with_cumsum.csv")


