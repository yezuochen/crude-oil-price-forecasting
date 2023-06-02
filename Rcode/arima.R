#### Crude Oil Price Forecast ####

### SARIMA ####
source("stationarity.R")

# package
library(tseries)
library(forecast)

# arima
auto.arima(crude_oil_price_WTI_mon[mon_train], trace = TRUE)
auto.arima(crude_oil_price_WTI_mon_gr[mon_train], trace = TRUE)

ma1 <- arima(crude_oil_price_WTI_mon, order = c(0,1,1), 
      seasonal = list(order = c(0,0,0), period = 12))

summary(ma1)

# prediction
pred <- Arima(crude_oil_price_WTI_mon[mon_test], model = ma1)[["fitted"]]

cbind(crude_oil_price_WTI_mon[mon_test],
      pred) %>% 
  ts.plot(col = c("red", "blue"), lwd = 2, xlab = "year", ylab = "price",
          main = "ARIMA(0,1,1) model Prediction in 2021-2022 (Test)")
legend("bottom", legend = c("Actual", "ARIMA"), col = c("red", "blue"),
       horiz = TRUE, lwd = 2, cex = 0.8)

rmse(coredata(crude_oil_price_WTI_mon[mon_test]), pred)


# ARIMA daily ####

auto.arima(crude_oil_price_WTI[date_train])
armaDay <- Arima(crude_oil_price_WTI[date_train], order = c(3, 1, 0))
summary(armaDay)

sqrt(armaDay$sigma2)

forecast(armaDay) %>% autoplot

# predict
predict(armaDay, n.ahead = 24)

pred <- Arima(crude_oil_price_WTI[date_test], model = armaDay)[["fitted"]] %>% 
  na.fill("extend")

pred <- zoo(pred, order.by = date_test) %>% 
  window(end = "2022-12-28")

# predict plot
cbind(crude_oil_price_WTI[date_test],
      pred) %>% 
  ts.plot(col = c("red", "blue"), lty = c(1,2),
          lwd = 2, xlab = "year", ylab = "price",
          main = "ARIMA(3,1,0) Model Daily Prediction in 2021-2022 (Test)", type = "l")
legend("bottom", legend = c("Actual", "ARIMA"), col = c("red", "blue"),
       horiz = TRUE, lwd = 2, cex = 0.8)

# test rmse
rmse(crude_oil_price_WTI[date_test], 
     pred)


# ARIMAX ####

xreg <- cbind(utility_rate, 
              indpro_d1,
              kilian_index_mon, 
              cpi_US,
              sp500_mon_d1, 
              dju_mon_d1,
              gold_mon_d1,
              gepu_d1) %>% 
  stats::lag(k=-1)
colnames(xreg) <- paste0(colnames(xreg), "_l1")

xreg_train <- xreg[mon_train]

auto.arima(crude_oil_price_WTI_mon[mon_train],
           xreg = xreg_train)

arimax <- Arima(crude_oil_price_WTI_mon[mon_train],
                      order = c(1, 0, 2), seasonal = c(0, 0, 0),
                      xreg = xreg_train)

summary(arimax)
sqrt(arimax$sigma2)

pred <- Arima(crude_oil_price_WTI_mon[mon_test],
              xreg = xreg[mon_test], model = arimax) %>% 
  .[["fitted"]]

cbind(crude_oil_price_WTI_mon[mon_test],
      pred) %>% 
  ts.plot(col = c("red", "blue"), lwd = 2, xlab = "year", ylab = "price",
          main = "ARIMAX(1,0,2) Model Prediction in 2021-2022 (Test)")
legend("bottom", legend = c("Actual", "ARIMAX"), col = c("red", "blue"),
       horiz = TRUE, lwd = 2, cex = 0.8)

rmse(crude_oil_price_WTI_mon[mon_test],
     Arima(crude_oil_price_WTI_mon[mon_test],
           xreg = xreg_arimax[mon_test], model = arimax212102)[["fitted"]])
