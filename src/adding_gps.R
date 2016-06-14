library(dplyr)

# load "gps.csv" and "dataset_polimi_predictions.csv" as dataset_polimi

# remove "Sottoarea_"
gps <- as.data.frame(sapply(gps, function(x) gsub("Sottoarea_", "", x)))

# rename columns

names(gps)[names(gps) == 'Sottoarea'] <- 'sottoarea'
names(gps)[names(gps) == 'LONGITUDINE'] <- 'longitudine'
names(gps)[names(gps) == 'LATITUDINE'] <- 'latitudine'



dataset <- left_join(dataset_polimi, gps)

write.csv(dataset_polimi, file="Modified data/dataset_polimi_complete.csv", row.names = FALSE)


