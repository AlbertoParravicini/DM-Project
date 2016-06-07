# Create historical series of product 1.
data_p1 <- filter(dataset, prod == 1, zona == 1) 
data_p1 <- data_p1[, c("data", "vendite")]
data_p1$vendite <- as.numeric(levels(data_p1$vendite))[data_p1$vendite]
data_p1 <- aggregate(cbind(vendite) ~ data , data = data_p1, FUN = sum)

data_p1$data <- as.Date(as.character(data_p1$data),format="%Y-%m-%d")
# create xts object
p1_ts <- xts(data_p1$vendite, data_p1$data)
rm(data_p1)

plot(p1_ts)
abline(lm(p1_ts~time(p1_ts)), lwd=30, col="red")


chart.TimeSeries(p1_ts, legend.loc="bottom", main=" ")
acf(p1_ts, lag.max = 400)
pacf(p1_ts, lag.max = 400)
