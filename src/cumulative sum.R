library(dplyr)

dataset <- read.csv("/dataset_polimi.csv", stringsAsFactors=FALSE, row.names=NULL)
# need to have run "data preparation.R" & "data preparation2.R" on this dataset!

# =========== CUMULATIVE SUM OF THE YEAR =============================
v_sp_anno <- vector(mode="numeric", length=0L)

for (k in unique(dataset$key)){
  for (p in unique(dataset$prod)){
    for (y in unique(dataset$anno)){
      # v_temp <- vector(mode="numeric", length=0L)
      temp <- filter(dataset, anno==y & key==k & prod==p)$vendite
      # print(c(k, y, length(temp)))
      if(length(temp)!=0){
       v_temp <- c(0, cumsum(temp))
       v_temp <- v_temp[1:(length(v_temp)-1)]
       v_sp_anno <- c(v_sp_anno, v_temp)
      }
    }
  }
}

dataset <- cbind(dataset, v_sp_anno)
names(dataset)[names(dataset) == 'v_sp_anno'] <- 'sp_anno'

# ============ CUMULATIVE SUM OF THE MONTH ==============================
v_sp_mese <- vector(mode="numeric", length=0L)

for (k in unique(dataset$key)){
  for ( p in unique(dataset$prod)){
    for (y in unique(dataset$anno)){
      # v_temp <- vector(mode="numeric", length=0L)
      temp <- filter(dataset, anno==y & key==k & prod==p)
      for(m in unique(dataset$mese)){
        # print(c(k, y, m))
        temp2 <- filter(temp, mese==m)$vendite
        if(length(temp2)!=0){
          v_temp <- c(0,cumsum(temp2))
          v_temp <- v_temp[1:(length(v_temp)-1)]
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

for (k in unique(dataset$key)){
  for (p in unique(dataset$prod)){
      for (y in unique(dataset$anno)){
        v_temp <- vector(mode="numeric", length=0L)
        temp <- filter(dataset, anno==y & key==k & prod==p)
        for(w in unique(dataset$settimana_anno)){
          # print(c(k, y, w))
          temp2 <- filter(temp, settimana_anno==w)$vendite
          if(length(temp2)!=0){
            v_temp <- c(0,cumsum(temp2))
            v_temp <- v_temp[1:(length(v_temp)-1)]
            v_sp_settimana <- c(v_sp_settimana, v_temp)
          }
      }
    }
  }
}

dataset <- cbind(dataset, v_sp_settimana)
names(dataset)[names(dataset) == 'v_sp_settimana'] <- 'sp_settimana'


# ============  WRITEBACK

write.csv(dataset, file="Modified data/dataset_polimi_final.csv")


