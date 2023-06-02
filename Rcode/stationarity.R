#### Crude Oil Price Forecast ####

### Plotting and Stationarity ####

source("../Rcode/import.R")

## Packages ####
library(TSstudio)
library(xts)
library(aTSA)
library(pracma)
library(mFilter)

## Split Data ####

date_train <- seq(as.Date("2011-01-01"), as.Date("2021-01-01"), by = 1)
date_test <- seq(as.Date("2021-01-02"), as.Date("2023-01-01"), by = 1)

rg_train <- as.yearmon(c("2011-01-01", "2021-01-01"))
rg_test <- as.Date(c("2021-01-02", "2023-01-01"))

mon_train <- as.yearmon(seq(as.Date("2011-01-01"), as.Date("2021-01-01"), by = "month"))
mon_test <- as.yearmon(seq(as.Date("2021-01-02"), as.Date("2023-01-01"), by = "month"))


## WTI Crude Oil Price ####

crude_oil_price_WTI_mon <- aggregate(crude_oil_price_WTI,
                                     as.yearmon, mean, na.rm = TRUE)

# to.monthly(crude_oil_price_WTI, OHLC = FALSE)
window(crude_oil_price_WTI_mon, 
       start = as.yearmon(rg_train[1]), end = as.yearmon(rg_train[2])) %>% 
  plot(ylab = "Price", xlab = "Year",
     main = "Monthly WTI Crude Oil Price")

acf(crude_oil_price_WTI_mon[mon_train], lag.max = 24, 
    main = "ACF Plot of Monthly WTI Crude Oil Price", xlab = "Year")
pacf(crude_oil_price_WTI_mon[mon_train], 
     main = "PACF Plot of Monthly WTI Crude Oil Price", xlab = "Year")

diff(crude_oil_price_WTI_mon[mon_train], lag = 1, differences = 1) %>% 
  plot(ylab = "Price", xlab = "Year",
       main = "Monthly WTI Crude Oil Price with First Order Difference")

crude_oil_price_WTI_mon_d1 <- diff(crude_oil_price_WTI_mon, lag = 1, differences = 1)

crude_oil_price_WTI_mon_gr <- diff(log(crude_oil_price_WTI_mon))


adf.test(coredata(crude_oil_price_WTI_mon[mon_train]))
adf.test(coredata(crude_oil_price_WTI_mon_d1[mon_train]))

plot.ts(crude_oil_price_WTI_mon_d1[mon_train])
plot.ts(crude_oil_price_WTI_mon_gr[mon_train])

# U.S. Crude Oil Production #### 

index(crude_oil_production_US) %<>% as.yearmon() 

crude_oil_production_US <-  na.fill(crude_oil_production_US, fill = "extend")

plot(crude_oil_production_US[mon_train],
     ylab = "Production", xlab = "Year",
     main = "Monthly U.S. Crude Oil Production")

crude_oil_production_US_dt <- zoo(detrend(as.numeric(coredata(crude_oil_production_US)),tt = "linear"),
                                  order.by = index(crude_oil_production_US))

plot(crude_oil_production_US_dt[mon_train],
     ylab = "Production", xlab = "Year",
     main = "Monthly U.S. Crude Oil Production with Detrend")

# loess(coredata(crude_oil_production_US[mon_train]) ~ time(crude_oil_production_US[mon_train]), span = 0.4)

adf.test(diff(coredata(crude_oil_production_US[mon_train])))
crude_oil_production_US_d1 <- diff(crude_oil_production_US, lag = 1, differences = 1)

acf(crude_oil_production_US_d1[mon_train])
pacf(crude_oil_price_WTI_mon_d1[mon_train])

plot(crude_oil_production_US_d1[mon_train],
     ylab = "Production", xlab = "Year",
     main = "Monthly U.S. Crude Oil Production with First Order Difference")

# Utility Rate ####

index(utility_rate) %<>% as.yearmon 
plot(utility_rate[mon_train], xlab = "Year", ylab = "Rate",
     main = "Capacity Utility Rate")

acf(utility_rate[mon_train])
pacf(utility_rate[mon_train])

adf.test(coredata(utility_rate[mon_train]))

# Weekly U.S. Ending Stocks excluding SPR of Crude Oil ####

stocks_US_mon <- aggregate(stocks_US, as.yearmon, mean, na.rm = TRUE)

plot(stocks_US_mon[mon_train], xlab = "Year", ylab = "Quantity",
     main = "Monthly U.S. Ending Stocks excluding SPR of Crude Oil")

acf(stocks_US_mon[mon_train])
pacf(stocks_US_mon[mon_train])
adf.test(coredata(stocks_US_mon[mon_train]))

stocks_US_mon_hpfilter <- hpfilter(stocks_US_mon, freq = 1600)
plot(stocks_US_mon_hpfilter$x, type = "l",
     main = "Stock of Crude Oil and Trend of HP Filter", ylab = "quantity")
lines(stocks_US_mon_hpfilter$trend, col = "red")
plot(stocks_US_mon_hpfilter$cycle, type = "l", ylab = "quantity",
     main = "Cycle of Stock of Crude Oil with HP Filter")

stocks_US_mon_hpfilter_cycle <- zoo(stocks_US_mon_hpfilter$cycle,
                                    order.by = index(stocks_US_mon))
adf.test(coredata(stocks_US_mon_hpfilter_cycle))

# Baltic Dirty Tanker (BAID) ####

baid_mon <- aggregate(baid, as.yearmon, mean, na.rm = TRUE)

plot(baid[date_train], xlab = "Year", ylab = "Index",
     main = "Daily Baltic Dirty Tanker (BAID)")
plot(baid_mon[mon_train], xlab = "Year", ylab = "Index",
     main = "Monthly Baltic Dirty Tanker (BAID)")

acf(baid_mon[mon_train])
pacf(baid_mon[mon_train])
adf.test(coredata(baid_mon[mon_train]))

md_dt_baid <- lm(baid_mon[mon_train] ~ index(baid_mon[mon_train])) 

dt_baid_mon <- md_dt_baid %>% 
  residuals() %>% 
  zoo(order.by = index(baid_mon[mon_train]))

dt_baid <- lm(baid[date_train] ~ index(baid[date_train])) %>% 
  residuals.lm() %>% 
  zoo(order.by = index(baid[date_train]))

plot(dt_baid[date_train], xlab = "Year", ylab = "Value",
     main = "Detrend Daily Baltic Dirty Tanker (BAID)")

adf.test(coredata(dt_baid))

adf.test(coredata(dt_baid_mon))

plot.ts(dt_baid_mon, xlab = "Year", ylab = "Value", main = "Detrend Monthly Baltic Dirty Tanker (BAID)")

# Product Supplied of Petroleum ####

index(petroleum) %<>% as.yearmon()

plot(petroleum[mon_train], xlab = "Year", ylab = "Quantity",
     main = "Product Supplied of Petroleum")

acf(petroleum[mon_train])
pacf(petroleum[mon_train])
adf.test(coredata(petroleum[mon_train]))

petroleum_dsd2 <- diff(petroleum, lag = 12, difference = 2)
plot(petroleum_dsd2[mon_train], xlab = "Year", ylab = "Quantity",
     main = "Product Supplied of Petroleum without Seasonality")

adf.test(coredata(petroleum_dsd2[mon_train]))
acf(petroleum_dsd2[mon_train], lag.max = 48)

# Industrial Production: Total Index ####

index(indpro) %<>% as.yearmon() 
plot.ts(indpro[mon_train], main = "Monly Industrial Production Index",
        xlab = "Year", ylab = "Index")

adf.test(coredata(indpro[mon_train]))

indpro_d1 <- diff(indpro, lag = 1, differences = 1)

plot.ts(indpro_d1[mon_train], xlab = "Year", ylab = "Value",
        main = "Differenced Monthly Industrial Production")

adf.test(coredata(indpro[mon_train])) # no solution for unit-root

# Kilian Global Economic Index ####

plot(kilian_index[date_train])

kilian_index_mon <- aggregate(kilian_index, as.yearmon, mean, na.rm = TRUE)

adf.test(coredata(kilian_index_mon[mon_train]))

acf(kilian_index_mon[mon_train])

# US CPI: Seasonally Adjusted ####

index(cpi_US) %<>% as.yearmon() 

adf.test(coredata(cpi_US[mon_train]))

# S&P 500 Index ####

sp500_mon <- aggregate(sp500, as.yearmon, mean, na.rm = TRUE)

plot.ts(sp500_mon[mon_train])

sp500_mon_d1 <- diff(sp500_mon)

plot.ts(sp500_mon_d1)

adf.test(coredata(sp500_mon_d1[mon_train]))

# DOW JONES UTILITY AVERAGE ####

dju_mon <- aggregate(dju, as.yearmon, mean, na.rm = TRUE)

adf.test(coredata(dju_mon[mon_train]))

plot.zoo(dju_mon[mon_train])

dju_mon_d1 <- diff(dju_mon)

adf.test(coredata(dju_mon_d1[mon_train]))

# CFTC Non-commercial Net Long ####

net_long_mon <- aggregate(net_long, as.yearmon, mean, na.rm = TRUE)

plot(net_long_mon[mon_train])

net_long_mon_d1 <- diff(net_long_mon)

adf.test(coredata(net_long_mon_d1[mon_train]))

# Gold Futures - Apr 23 (GCJ3) ####

gold_mon <- aggregate(gold, as.yearmon, mean, na.rm = TRUE)

plot(gold_mon[mon_train])

gold_mon_d1 <- diff(gold_mon)

adf.test(coredata(gold_mon_d1[mon_train]))

# Money Market Funds; Total Financial Assets, Level ####

money_funds_mon <- aggregate(money_funds, as.yearmon, mean, na.rm = TRUE)

plot.ts(money_funds_mon[mon_train])

money_funds_mon_d1 <- diff(money_funds_mon)

adf.test(coredata(money_funds_mon_d1[mon_train]))

# Global Economic Policy Uncertainty Index ####
# without parity

index(gepu) %<>% as.yearmon()

plot.ts(gepu)

adf.test(coredata(gepu[mon_train]))

gepu_d1 <- diff(gepu)

adf.test(coredata(gepu_d1[mon_train]))

