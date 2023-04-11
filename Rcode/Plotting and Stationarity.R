#### Crude Oil Price Forecast ####

### Plotting and Stationarity ####

## Packages ####
library(TSstudio)
library(xts)
library(aTSA)
library(pracma)
library(mFilter)

## Split Data ####

date_train <- seq(as.Date("2005-01-01"), as.Date("2015-01-01"), by = 1)
date_verify <- seq(as.Date("2015-01-02"), as.Date("2020-01-01"), by = 1)
date_test <- seq(as.Date("2020-01-02"), as.Date("2023-01-01"), by = 1)

rg_train <- as.yearmon(c("2005-01-01", "2015-01-01"))
rg_verify <- as.Date(c("2015-01-02", "2020-01-01"))
rg_test <- as.Date(c("2020-01-02", "2023-01-01"))

mon_train <- as.yearmon(seq(as.Date("2005-01-01"), as.Date("2015-01-01"), by = "month"))
mon_verify <- as.yearmon(seq(as.Date("2015-01-02"), as.Date("2020-01-01"), by = "month"))
mon_test <- as.yearmon(seq(as.Date("2020-01-02"), as.Date("2023-01-01"), by = "month"))

## Fill NA ####
list_zoo <- lapply(list_zoo, na.fill, fill = "extend")

names(list_zoo[1])

crude_oil_price_WTI <- apply.monthly(crude_oil_price_WTI, mean, na.rm = TRUE)

comment(crude_oil_price_WTI)


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

adf.test(coredata(crude_oil_price_WTI_mon[mon_train]))
adf.test(coredata(crude_oil_price_WTI_mon_d1[mon_train]))

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



#########################################################################
## Plot ####

setwd("C:/Users/PC622R/OneDrive - 中華經濟研究院/Price Forecasting Model/Result/time series plot")


## Plot with List of Zoo ####
plt.list.zoo <- function(x, rg, y){
  nam <- names(x)
  x_rg <- window(x, start = rg[1], end = rg[2], extend = TRUE)
  png(filename = paste0(nam, ".png"), 
      width = 6, height = 4, units = "in", res = 1080)
  plot.zoo(x_rg,
           xlab = "Year", ylab = "Price / Index",
           main = nam,
           xaxt = "n")
  axis.Date(side = 1, at = as.Date(y, format = "%Y"), format = "%Y")
  dev.off()
}


lapply(list_zoo, plt.list.zoo, rg = rg_train, y = y_train)

plt.list.zoo(x = list_zoo[5], rg = rg_train, y = y_train)
plot.zoo(mulzoo, xlim = rg_test)

lapply(list_zoo, window, start = rg_train[1], end = rg_train[2]) %>% 
  acf

## ACF and PACF Plot ####

acf(crude_oil_price_WTI, na.action = na.pass, lag.max = 100,
    main = "ACF Plot of WTI Crude Oil Price")
pacf(crude_oil_price_WTI, na.action = na.pass, lag.max = 40,
     main = "PACF Plot of WTI Crude Oil Price")
plot.zoo(crude_oil_price_WTI)


names(mulzoo)
comment(mulzoo)


## Test ####
############################################################################
plot.ts(df$crude_oil_price_WTI)
ts.plot(ts(df$crude_oil_price_WTI), 
        main = "Crude Oil Price (WTI)", ylab = "price")



window(df, start = "2005-01-01", end = "2014-12-31")

df[date_train]
plot.ts(df[date_train]$crude_oil_price_WTI)
df["2005/2014"]
df_train <- window(df, start = "2005-01-01", end = "2014-12-31")

ts_plot(df_train$crude_oil_price_WTI)
plot.ts(df_train$crude_oil_price_WTI, lwd = 2)
plot.zoo(df_train$crude_oil_price_WTI)
plot.zoo(df$crude_oil_price_WTI, xlab = "Date", ylab = "Price",
         xlim = as.Date(c("2005-01-01", "2014-12-31")), ylim = c(0, 200))
axis(side = 1, at = as.Date(c("2005-01-01", "2014-12-31")))

plot(df_train$crude_oil_price_WTI, ylim = c(min(df_train$crude_oil_price_WTI, na.rm = TRUE),
                                            max(df_train$crude_oil_price_WTI, na.rm = TRUE)))
plot.zoo(df$crude_oil_price_WTI, xlim = as.Date(c("2005-01-01", "2014-12-31")), ylim = c(0,200))


df_train <- na.locf(df_train)

range(df_train, na.rm = TRUE)
summarise_all(data.frame(df_train), range, na.rm = TRUE)
plot(df_train$gepu)
na.approx(df_train) %>% plot.zoo

plot.zoo(crude_oil_price_WTI, xlim = rg_train, xlab = "Date", ylab = "Price", 
         main = toupper(gsub("_", " ", expression(crude_oil_price_WTI))), xaxt = "n")
axis.Date(1, seq(as.Date("2005-01-01"), as.Date("2014-12-31"), by = "year"), format = "%Y")

axis(side = 1, format(index(crude_oil_price_WTI), "%y"))
plot(crude_oil_price_WTI)

#######################################################################################

         