library(xts)
library(ggplot2)
library(dplyr)
library(tseries)
library(PerformanceAnalytics)
library(forecast)
library(astsa)
library(Metrics)


# DATA IMPORT AND CLEANING
# ------------------------------------------------------
# ------------------------------------------------------

setwd("~/DM-Project")
dataset <- read.csv("~/DM-Project/Modified data/dataset_polimi.csv", stringsAsFactors=FALSE, row.names=NULL)
# Remove the x column, if present
dataset <- dataset[ , !(names(dataset) %in% c("X"))]

# Build a smaller datasetset, for testing
# dataset <- dataset[sample(1:nrow(dataset), 1000), ]

# Convert dates to class "Data"
dataset$data <- as.Date(dataset$data)
# Convert "vendite" to numeric values if needed
if (class(dataset$vendite) == "factor") {
  dataset$vendite <- as.numeric(levels(dataset$vendite))[dataset$vendite]
}

# Turn some features to factors
factorVars <- c('zona','area', "sottoarea",
                'prod','giorno_mese', "giorno_settimana", "giorno_anno", "mese", "settimana_anno", "anno", "weekend","stagione", "key")

dataset[factorVars] <- lapply(dataset[factorVars], function(x) as.factor(x))

summary(dataset)

# ------------------------------------------------------
# ------------------------------------------------------

# Plot sales for each day of the week
daily <- aggregate(cbind(vendite) ~ giorno_settimana , data = dataset, FUN = sum)
barplot(daily$vendite)
# Can we do better?
p0 <- ggplot(dataset, aes(giorno_settimana, vendite)) + geom_boxplot() + facet_grid(. ~ prod)
p0
# Sales on monday seems to be lower than the other days, but there are many high outliers.
# Remove the outliers
# compute lower and upper whiskers
ylim1 = boxplot.stats(dataset$vendite)$stats[c(1, 5)]
# scale y limits based on ylim1
p1 <- p0 + coord_cartesian(ylim = ylim1*1.05)
p1
# As expected, not many sales in weekends. The sales during the week are more or less constant

# Plot sales for each month
daily <- aggregate(cbind(vendite) ~ mese , data = dataset, FUN = sum)
barplot(daily$vendite)
# Can we do better?
p0 <- ggplot(dataset, aes(mese, vendite)) + geom_boxplot()
p0
# There are fewer slase in august, but not by a wide margin
# The higher sales in february might be linked to the smaller number of days
# There  are a lot of outliers in june!

# Remove the outliers
# compute lower and upper whiskers
ylim1 = boxplot.stats(dataset$vendite)$stats[c(1, 5)]
# scale y limits based on ylim1
p1 <- p0 + coord_cartesian(ylim = ylim1*1.05)
p1

# Plot sales for each day of the month
daily <- aggregate(cbind(vendite) ~ giorno_mese , data = dataset, FUN = sum)
barplot(daily$vendite)
# Can we do better?
p0 <- ggplot(dataset, aes(giorno_mese, vendite)) + geom_boxplot() + facet_grid(. ~ prod)
p0
# Remove the outliers
# compute lower and upper whiskers
ylim1 = boxplot.stats(dataset$vendite)$stats[c(1, 5)]
# scale y limits based on ylim1
p1 <- p0 + coord_cartesian(ylim = ylim1*1.05)
p1
# Fewer sales on the first of the month! why?
# maybe the considered years had a lot of sundays on the first?
for (i in 1:7) {
  cat(nrow(filter(dataset, giorno_settimana == i, giorno_mese == 1, sottoarea=="5")), ", ")
}
# Not by a wide margin, but their number is still above average

