library(dplyr)

dataset <- read.csv("~/DM-Project/dataset_polimi.csv", stringsAsFactors=FALSE, row.names=NULL)
# need to have run "data preparation.R" on this dataset!

# =========== CUMULATIVE SUM OF THE YEAR =============================
v_sp_anno <- vector(mode="numeric", length=0L)

for (k in unique(dataset$Key)){
  for (p in unique(dataset$Categoria_prodotto)){
    for (y in unique(dataset$Anno)){
      v_temp <- vector(mode="numeric", length=0L)
      cumsum(0)
      temp <- filter(dataset, Anno==y & Key==k & Categoria_prodotto==p)$Vendite
      # print(c(k, y, length(temp)))
      if(length(temp)!=0){
       v_temp <- cumsum(temp) 
       v_sp_anno <- c(v_sp_anno, v_temp)
      }
    }
  }
}

dataset <- cbind(dataset, v_sp_anno)
names(dataset)[names(dataset) == 'v_sp_anno'] <- 'sp_anno'

# ============ CUMULATIVE SUM OF THE MONTH ==============================
v_sp_mese <- vector(mode="numeric", length=0L)

for (k in unique(dataset$Key)){
  for ( p in unique(dataset$Categoria_prodotto)){
    for (y in unique(dataset$Anno)){
      v_temp <- vector(mode="numeric", length=0L)
      temp <- filter(dataset, Anno==y & Key==k & Categoria_prodotto==p)
      for(m in unique(dataset$Mese)){
        # print(c(k, y, m))
        temp2 <- filter(temp, Mese==m)$Vendite
        if(length(temp2)!=0){
          v_temp <- cumsum(temp2) 
          v_sp_mese <- c(v_sp_mese, v_temp)
        }
      }
    }
  }
}

dataset <- cbind(dataset, v_sp_mese)
names(dataset)[names(dataset) == 'v_sp_mese'] <- 'sp_mese'

# ============ CUMULATIVE SUM OF THE WEEK ==============================
v_sp_settimana <- vector(mode="numeric", length=0L)

for (k in unique(dataset$Key)){
  for (p in unique(dataset$Categoria_prodotto)){
      for (y in unique(dataset$Anno)){
        v_temp <- vector(mode="numeric", length=0L)
        temp <- filter(dataset, Anno==y & Key==k & Categoria_prodotto==p)
        for(w in unique(dataset$Settimana_Anno)){
          # print(c(k, y, w))
          temp2 <- filter(temp, Settimana_Anno==w)$Vendite
          if(length(temp2)!=0){
            v_temp <- cumsum(temp2) 
            v_sp_settimana <- c(v_sp_settimana, v_temp)
          }
      }
    }
  }
}

dataset <- cbind(dataset, v_sp_settimana)
names(dataset)[names(dataset) == 'v_sp_settimana'] <- 'sp_settimana'


# ============  WRITEBACK

write.csv(dataset, file="Modified data/dataset_polimi_with_cumsum.csv")


