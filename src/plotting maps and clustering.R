#============= PLOTTING ALL POINTS ================================================

library(rworldmap)
newmap3 <- getMap(resolution = "low")
plot(newmap3, xlim = c(-10, 30), ylim = c(40, 50), asp = 1)
points(gps$LONGITUDINE, gps$LATITUDINE, col = "red", cex = .2)

#================= PLOTTING FUNNY MAPS (too many maps!!!!) ============================================
# plots the coords of each zona

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



# ================ HIERARCHICAL CLUSTERING =======================================

library(GMD)


# init the seed to be able to repeat the experiment
set.seed(1234)
par(mar=c(0,0,0,0))
# randomly generates the data
x<-unique(dataset_polimi_complete$latitudine)
y<-unique(dataset_polimi_complete$longitudine)
plot(x,y,pch=19,cex=2,col="blue")
# distance matrix
d <- data.frame(x,y)
dm <- dist(d)
# generate the
cl <- hclust(dm)
plot(cl)

###
### checking the quality of the previous cluster
###
# init two vectors that will contain the evaluation
# in terms of within and between sum of squares
num_points <- 144
plot_points <- 20

plot_wss = rep(0,plot_points)
plot_bss = rep(0,plot_points)
# evaluate every clustering
for(i in 1:plot_points)
{
  clusters <- cutree(cl,i)
  eval <- css(dm,clusters);
  plot_wss[i] <- eval$totwss
  plot_bss[i] <- eval$totbss
}
# plot the results
x = 1:plot_points
plot(x, y=plot_bss, main="Between Cluster Sum-of-square",
     cex=2, pch=18, col="blue", xlab="Number of Clusters",
     ylab="Evaluation")
lines(x, plot_bss, col="blue")
par(new=TRUE)
plot(x, y=plot_wss, cex=2, pch=19, col="red", ylab="", xlab="")
lines(x,plot_wss, col="red")

# a good candidate might be 5 or 6 clusters 


#================= KMEANS CLUSTERING ====================================================

set.seed(1234)

temp_dataset <- dataset_polimi_complete

k <- c(3,6,20) #num of clusters
for(i in k){
  cluster <- kmeans(dataset_polimi_complete[,c("latitudine", "longitudine")], center=k)
  temp_dataset <- cbind(temp_dataset, cluster$cluster)
}
library(dplyr)
library(rworldmap)

names(temp_dataset)[names(temp_dataset) == 'cluster$cluster'] <- 'cluster20'

map <- getMap(resolution = "low")
plot(map, xlim = c(-10, 30), ylim = c(40, 50), asp = 1)


# need to change "cluster" to "clusterK" where k is the number of clusters
# for (i in seq(1:k)){
#   for (s in unique(filter(temp_dataset, cluster==i)$sottoarea)){
#     # print(s)
#     temp <- filter(temp_dataset, sottoarea==s)
#     points(temp$longitudine[1], temp$latitudine[1], col=i, asp=1)
#   }
# }


#======= SAVE STUFF ========================
write.csv(temp_dataset, file="Modified data/dataset_polimi_clusterized.csv", row.names = FALSE)

