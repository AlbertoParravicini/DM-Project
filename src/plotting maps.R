#============= PLOTTING ALL POINTS ================================================

library(rworldmap)
newmap3 <- getMap(resolution = "low")
plot(newmap3, xlim = c(-10, 30), ylim = c(40, 50), asp = 1)
points(gps$LONGITUDINE, gps$LATITUDINE, col = "red", cex = .2)

#================= PLOTTING FUNNY MAPS ============================================

library(dplyr)
library(rworldmap)

i <- 1
for (i in unique(dataset_polimi_complete$zona)){
  map <- getMap(resolution = "low")
  plot(map, xlim = c(-10, 30), ylim = c(40, 50), asp = 1)
  for (s in unique(filter(dataset_polimi_complete, zona==i)$sottoarea)){
    # print(s)
    temp <- filter(dataset_polimi_complete, sottoarea==s)
    points(temp$longitudine[1], temp$latitudine[1], col=i, asp=1)
  }
  i <- i+1
}

#================= KMEANS CLUSTERING ====================================================
k <- 6 #num of clusters

cluster <- kmeans(dataset_polimi_complete[,c("latitudine", "longitudine")], center=k)
dataset2 <- cbind(dataset_polimi_complete, cluster$cluster)

library(dplyr)
library(rworldmap)

names(dataset2)[names(dataset2) == 'cluster$cluster'] <- 'cluster'

map <- getMap(resolution = "low")
plot(map, xlim = c(-10, 30), ylim = c(40, 50), asp = 1)

for (i in seq(1:k)){
  for (s in unique(filter(dataset2, cluster==i)$sottoarea)){
    # print(s)
    temp <- filter(dataset2, sottoarea==s)
    points(temp$longitudine[1], temp$latitudine[1], col=i, asp=1)
  }
}

