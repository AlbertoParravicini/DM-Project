library(xts)
library(ggplot2)
library(dplyr)
library(tseries)
library(PerformanceAnalytics)
library(forecast)
library(astsa)
library(Metrics)
library(normwhn.test)
library(nortest)
library(nortestARMA)

dataset_polimi <- read.csv("~/DM-Project/Original data/dataset_polimi.csv", stringsAsFactors=FALSE)

dataset_polimi$data <- as.Date(as.character(dataset_polimi$Data),format="%Y-%m-%d")

subarea_20_p1 <- filter(dataset_polimi, Sottoarea == "Sottoarea_20", Categoria_prodotto == "Prodotto_1")

subarea_20_p2 <- filter(dataset_polimi, Sottoarea == "Sottoarea_20", Categoria_prodotto == "Prodotto_2")

tsdisplay(subarea_20_p1$Vendite, lag.max = 100, main = "Analysis of subarea 20 - Product 1")

tsdisplay(subarea_20_p2$Vendite, lag.max = 100, main = "Analysis of subarea 20 - Product 2")

# Are these two time series white noise (or deterministic signals)?
# Null hypothesis: the signal is made of independent observations.
# Low p-value -> we can safely reject the null hypothesis
Box.test(subarea_20_p1$Vendite)
#X-squared = 0.057371, df = 1, p-value = 0.8107

Box.test(subarea_20_p2$Vendite)
#X-squared = 0.0046295, df = 1, p-value = 0.9458

# Just for comparison:
Box.test(rnorm(nrow(subarea_20_p1)))
#X-squared = 0.96952, df = 1, p-value = 0.3248

Box.test(filter(dataset_polimi, Sottoarea == "Sottoarea_22", Categoria_prodotto == "Prodotto_1")$Vendite)
#X-squared = 88.688, df = 1, p-value < 2.2e-16



# Are there other subarea with weird behaviours?

options(scipen=5)
res <- data.frame(matrix(NA, ncol = 5, nrow = 0))
# For each subarea, display mean, median, variance of sales.
for (sub_i in sort(unique(dataset_polimi$Sottoarea))) {
  for (prod_i in sort(unique(dataset_polimi$Categoria_prodotto))) {
    temp_data <- filter(dataset_polimi, Sottoarea == sub_i, Categoria_prodotto == prod_i)
    temp_row <- data.frame(Sottoarea = sub_i, prod = prod_i, media = mean(temp_data$Vendite), median = median(temp_data$Vendite), var = var(temp_data$Vendite), p.value = Box.test(temp_data$Vendite)$p.value)
    print(temp_row)
    res <- rbind(res, temp_row)
  }
}
View(res)

# Subarea 78 and 32, relatively to product 2, can be considered deterministic signals with value = 0

write.csv(res, "Results/Subarea analysis.csv", row.names = F)










