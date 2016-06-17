library(dplyr)

# load "gps.csv" and "dataset_polimi_predictions.csv" as dataset_polimi

# remove "Sottoarea_"
gps <- as.data.frame(sapply(gps, function(x) gsub("Sottoarea_", "", x)))

# rename columns

names(gps)[names(gps) == 'Sottoarea'] <- 'sottoarea'
names(gps)[names(gps) == 'LONGITUDINE'] <- 'longitudine'
names(gps)[names(gps) == 'LATITUDINE'] <- 'latitudine'


if (class(gps$sottoarea)=="factor"){
  gps$sottoarea <- as.numeric(levels(gps$sottoarea))[gps$sottoarea]
}


dataset <- left_join(dataset_polimi_complete, gps)

write.csv(dataset, file="Modified data/dataset_polimi_complete.csv", row.names = FALSE)


