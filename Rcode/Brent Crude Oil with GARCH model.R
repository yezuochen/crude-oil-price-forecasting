                   #### Brent Crude Oil with GARCH Model ####
library(readr)
library(zoo)
library(fGarch)
library(PerformanceAnalytics)
library(rugarch)
library(tseries)
library(xts)
library(FinTS)
library(urca)
library(dplyr)
library(lubridate)

brent <- read_csv("C:/Users/PC622R/OneDrive - 中華經濟研究院/Price Forecasting Model/Data/brent-crude-oil-prices-10-year-daily-chart.csv", 
			skip = 15)

brent <- mutate(brent, date = ymd(date))

brent.z <- zoo(x = brent$value, order.by = brent$date)

# calculate log return and remove first NA

Return.brent <- Return.calculate(brent.z, method = "log")[-1]

# ADF test with drift

ADF_Returns <- ur.df(Return.brent, type = "drift", selectlags = "AIC")

summary(ADF_Returns)


dataToPlot <- cbind(Return.brent, Return.brent^2, abs(Return.brent))

colnames(dataToPlot) <- c("Return", "Return^2", "abs(Return)")

plot.zoo(dataToPlot, main = "Brent Crude Oil Daily Return", col = "blue")






