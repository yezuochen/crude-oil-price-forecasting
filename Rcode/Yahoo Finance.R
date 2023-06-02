#####  How to download stock prices in R  #####

# Getting stock prices from Yahoo Finance ####

library(tidyquant)
library('TTR')
library('quantmod')
library(ggplot2)
library(tseries)
library(purrr)
library(forecast)

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

# Downloading Apple price using quantmod

# getSymbols("AAPL", from = "2000-01-01",
#            to = Sys.Date(),warnings = FALSE,
#            auto.assign = TRUE)
# 
# head(AAPL)
# tail(AAPL)
# str(AAPL)
# 
# getSymbols("SPX", from = "2000-01-01",
#            to = Sys.Date(),warnings = FALSE,
#            auto.assign = TRUE)
# 
# head(SPX)
# str(SPX)

clf <- getSymbols("CL=F", from = "2005-01-01", 
           to = Sys.Date(), warings = FALSE,
           auto.assign = TRUE)
chartSeries(`CL=F`, subset="last 6 months", theme=chartTheme("white"))

clf <- tq_get("CL=F", from = "2005-01-01", to = Sys.Date())
tail(clf)

rownames(clf)
str(clf)

clf <- zoo(clf$close, order.by = clf$date) %>% 
  na.fill(fill = "extend")

autoplot.zoo(clf)

ggplot(aes(x = Index, y = Value), data = fortify(clf, melt = TRUE)) +
  geom_line() +
  xlab("Index") +
  ylab("Value") +
  labs(title = "Crude Oil Jun 23 (CL=F)",
       subtitle = "NY Mercantile - NY Mercantile Delayed Price. Currency in USD",
       caption = "yahoo finance") +
  theme_tq()

plot(clf, xlab = "Year", ylab = "Price",
        main = "WTI Crude Oil Price Series Plot (CL=F)")

adf.test(clf)
acf(clf, na.action = na.pass, lag.max = 120)

clf_d1 <- diff(clf)
plot.ts(clf_d1, ylab = "Value", main = "Differenced Crude Oil Series Plot")

acf(clf_d1, na.action = na.pass, lag.max = 120)
pacf(clf_d1, na.action = na.pass, lag.max = 120)

auto.arima(clf_d1, stationary = TRUE)
auto.arima(clf)

arima410 <- arima(clf, order = c(4,1,0))
arima310 <- arima(clf, order = c(3,1,0))
arima511 <- arima(clf, order = c(5,1,1))

Box.test(arima410$residuals, lag = 11) %>% 
  pluck("p.value")

lgbox_plot <- function(res, lag){
  Box.test(res, lag = lag) %>% 
    pluck("p.value")
}

map_dbl(1:36, lgbox_plot, res = arima410$residuals) %>% 
  plot(main = "P-value for Ljung-Box Statistic of ARIMA(4,1,0)", type = "p",
       xlab = "Lag", ylab = "p-value") +
  abline(h = 0.05, col = "blue", lty = 2)

map_dbl(1:36, lgbox_plot, res = arima310$residuals) %>% 
  plot(main = "P-value for Ljung-Box Statistic of ARIMA(3,1,0)", type = "p",
       xlab = "Lag", ylab = "p-value") +
  abline(h = 0.05, col = "blue", lty = 2)

map_dbl(1:36, lgbox_plot, res = arima511$residuals) %>% 
  plot(main = "P-value for Ljung-Box Statistic of ARIMA(5,1,1)", type = "p",
       xlab = "Lag", ylab = "p-value") +
  abline(h = 0.05, col = "blue", lty = 2)


plot(arima410$residuals)
plot(arima310$residuals)
plot(arima511$residuals)

pred <- predict(arima511, n.ahead = 10)

plot(pred$pred)

clf.p <- rbind(clf, pred$pred)

window(clf.p, start = floor_date(Sys.Date(), "year")) %>% 
  plot(main = "Crude Oil Price Series and Prediction", 
       sub = "source: yahoo finance",
       ylab = "price", xlab = "month", lwd = 2) +
  abline(v = Sys.Date()) + 
  lines(window(clf.p, start = Sys.Date()), col = "red", lwd = 3) +
  lines(window(clf.p, start = Sys.Date()) + 1*pred$se, 
        col = "blue", lwd = 1) +
  lines(window(clf.p, start = Sys.Date()) - 1*pred$se, 
        col = "blue", lwd = 1)
  
