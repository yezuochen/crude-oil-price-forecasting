#### Crude Oil Price Forecast ####

### VAR ####
source("stationarity.R")

# packages 
library(MTS)
library(Metrics)
library(tsDyn)
library(tseries)
library(forecast)

# regressor
var_reg <- cbind(crude_oil_price_WTI_mon_d1, 
                 crude_oil_production_US_d1, 
                 utility_rate, 
                 stocks_US_mon_hpfilter_cycle,
                 #dt_baid_mon,
                 petroleum_dsd2, 
                 indpro_d1,
                 kilian_index_mon, 
                 cpi_US,
                 sp500_mon_d1, 
                 dju_mon_d1, 
                 net_long_mon_d1, 
                 gold_mon_d1,
                 money_funds_mon_d1,
                 gepu_d1) %>% 
  na.fill(fill = 0)

## VAR, using MTS ####

# fit all p
var_p1 <- MTS::VAR(coredata(var_reg[mon_train]), p = 1)
var_p2 <- MTS::VAR(coredata(var_reg[mon_train]), p = 2)
var_p3 <- MTS::VAR(coredata(var_reg[mon_train]), p = 3)
var_p4 <- MTS::VAR(coredata(var_reg[mon_train]), p = 4)
var_p5 <- MTS::VAR(coredata(var_reg[mon_train]), p = 5)

VARpred(var_p1, 1)

# select p
VARorder(coredata(var_reg[mon_train]), maxp = 6)

# predict(var_p1$coef, coredata(var_zoos[mon_verify]))

# coefficients table 
vars::VAR(var_reg[mon_train], ic = "AIC", lag.max = 2) %>% 
  .[["varresult"]] %>% 
  .[[1]] %>% 
  .[["coefficients"]] %>% 
  as.table %>% 
  round(4) %>% 
  write.table(quote = FALSE, row.names = FALSE)

# VAR, with tsDyn ####

# fit p  
var1_dyn <- tsDyn::lineVar(coredata(var_reg[mon_train]), lag = 1)
var2_dyn <- tsDyn::lineVar(coredata(var_reg[mon_train]), lag = 2)

# equation
summary(var1_dyn)
summary(var2_dyn)

# rmse
sqrt(sum((var1_dyn$residuals[,"crude_oil_price_WTI_mon_d1"])^2)/length(mon_train))
sqrt(sum((var2_dyn$residuals[,"crude_oil_price_WTI_mon_d1"])^2)/length(mon_train))

# predict var(1)
pred1 <- matrix(ncol = 14, nrow = 0)
for(i in 1:(length(mon_test))){
  p <- predict(var1_dyn, coredata(var_reg[mon_test[i]]), n.ahead = 1) 
  pred1 <- rbind(pred1, p)
}

# predictvar(2)
pred2 <- matrix(ncol = 14, nrow = 0)
for(i in 1:(length(mon_test)-1)){
  p <- predict(var2_dyn, coredata(var_reg[mon_test[c(i, i+1)]]), n.ahead = 1) 
  pred2 <- rbind(pred2, p)
}

# test predict plot
ts.plot(cbind(zoo(pred1[, "crude_oil_price_WTI_mon_d1"], order.by = mon_test[-1]),
              zoo(pred2[, "crude_oil_price_WTI_mon_d1"], order.by = mon_test[-c(1,2)]),
              crude_oil_price_WTI_mon_d1[mon_test]),
        col = c("blue", "green", "red"), lwd = 2,
        main = "VAR(1) and VAR(2) Difference Prediction in 2019-2023 (Test)",
        xlab = "Year", ylab = "value")
legend("bottom", legend = c("VAR(1)", "VAR(2)", "Actual"), col = c("blue", "green", "red"), 
       xpd = FALSE, lwd = 2, horiz = TRUE, cex = 0.5)

# price predict plot
cbind(crude_oil_price_WTI_mon[mon_test],
      stats::lag(crude_oil_price_WTI_mon[mon_test], k = -1) + 
        zoo(pred1[, "crude_oil_price_WTI_mon_d1"], order.by = mon_test[-1]),
      stats::lag(crude_oil_price_WTI_mon[mon_test], k = -2) + 
        zoo(pred2[, "crude_oil_price_WTI_mon_d1"], order.by = mon_test[-c(1,2)])) %>% 
  ts.plot(main = "VAR(1) and VAR(2) Prediction in 2021-2022 (Test)",
          col = c("red", "blue", "green"), lwd = 2,
          xlab = "Year", ylab = "value")
legend("bottom", legend = c( "Actual", "VAR(1)", "VAR(2)"), col = c("red", "blue", "green"), 
       xpd = FALSE, lwd = 2, horiz = TRUE, cex = 0.5)

# rmse
rmse(crude_oil_price_WTI_mon_d1[mon_test], 
     zoo(pred1[, "crude_oil_price_WTI_mon_d1"], order.by = mon_test[-1]))
rmse(crude_oil_price_WTI_mon_d1[mon_test], 
     zoo(pred2[, "crude_oil_price_WTI_mon_d1"], order.by = mon_test))

sd(crude_oil_price_WTI_mon_d1[mon_test])

