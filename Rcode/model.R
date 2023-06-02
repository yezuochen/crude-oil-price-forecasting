#### Crude Oil Price Forecast ####

### Modeling ####
source("stationarity.R")

## Packages ####

library(MTS)
library(Metrics)
library(tsDyn)
library(tseries)
library(forecast)
library(keras)
library(reticulate)
library(ggplot2)
# library(ggtheme)
library(mfGARCH)
library(fGarch)
library(data.table)
library(rumidas)
library(PerformanceAnalytics)
library(wavelets)
library(rugarch)
library(marked)

# input construction ####

x <- cbind(crude_oil_price_WTI_mon_gr, 
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
           gepu_d1)

x <- na.fill(x, fill = 0)

input_lag <- function(m = x_train, lag = 3){
  lags <- seq(0,lag)
  m_lag <- function(l, m=m){lag(m,l)}
  list_lag <- lapply(lags, m_lag, m=m)
  
  for(i in lags){
    colnames(list_lag[[i+1]]) <- paste0(colnames(list_lag[[i+1]]), "_lag", i)
  }
  
  m_lags <- list_lag %>% 
    do.call(cbind,.) %>% 
    window(start = index(m)[lag+1])
  
  as.data.frame(m_lags) %>% 
    mutate_all(.funs = function(x) (x-min(x)) / (max(x)-min(x))) %>% 
    select(!contains("crude_oil_price_WTI_mon_gr_lag0")) %>% 
    as.matrix()
}

## VAR, with MTS ####

var_zoos <- cbind(crude_oil_price_WTI_mon_d1, 
                  crude_oil_production_US_d1, 
                  utility_rate, 
                  stocks_US_mon_hpfilter_cycle, 
                  petroleum_dsd2)

var_p1 <- MTS::VAR(coredata(var_zoos[mon_train]), p = 1)
var_p2 <- MTS::VAR(coredata(var_zoos[mon_train]), p = 2)
var_p3 <- MTS::VAR(coredata(var_zoos[mon_train]), p = 3)
var_p4 <- MTS::VAR(coredata(var_zoos[mon_train]), p = 4)
var_p5 <- MTS::VAR(coredata(var_zoos[mon_train]), p = 5)

VARpred(var_p1, 1)

VARorder(coredata(var_zoos[mon_train]))

# predict(var_p1$coef, coredata(var_zoos[mon_verify]))

vars::VAR(var_zoos[mon_train], ic = "AIC", lag.max = 3) %>% 
  summary()

# VAR, with tsDyn ####

var1_dyn <- tsDyn::lineVar(coredata(var_zoos[mon_train]), lag = 1)
var2_dyn <- tsDyn::lineVar(coredata(var_zoos[mon_train]), lag = 2)

summary(var1_dyn)
summary(var2_dyn)

sqrt(sum((var1_dyn$residuals[,"crude_oil_price_WTI_mon_d1"])^2)/length(mon_train))
sqrt(sum((var2_dyn$residuals[,"crude_oil_price_WTI_mon_d1"])^2)/length(mon_train))

pred1 <- matrix(ncol = 5, nrow = 0)
for(i in 1:(length(mon_test))){
  p <- predict(var1_dyn, coredata(var_zoos[mon_test[i]]), n.ahead = 1) 
  pred1 <- rbind(pred1, p)
}

pred2 <- matrix(ncol = 5, nrow = 0)
for(i in 1:(length(mon_test)-1)){
  p <- predict(var2_dyn, coredata(var_zoos[mon_test[c(i, i+1)]]), n.ahead = 1) 
  pred2 <- rbind(pred2, p)
}

ts.plot(cbind(zoo(pred1[, "crude_oil_price_WTI_mon_d1"], order.by = mon_test[-1]),
              zoo(pred2[, "crude_oil_price_WTI_mon_d1"], order.by = mon_test[-c(1,2)]),
              crude_oil_price_WTI_mon_d1[mon_test]),
        col = c("blue", "green", "red"), lwd = 2,
        main = "VAR(1) and VAR(2) Difference Prediction in 2019-2023 (Test)",
        xlab = "Year", ylab = "value")
legend("bottom", legend = c("VAR(1)", "VAR(2)", "Actual"), col = c("blue", "green", "red"), 
       xpd = FALSE, lwd = 2, horiz = TRUE, cex = 0.5)

cbind(crude_oil_price_WTI_mon[mon_test],
      stats::lag(crude_oil_price_WTI_mon[mon_test], k = -1) + 
        zoo(pred1[, "crude_oil_price_WTI_mon_d1"], order.by = mon_test[-1]),
      stats::lag(crude_oil_price_WTI_mon[mon_test], k = -2) + 
        zoo(pred2[, "crude_oil_price_WTI_mon_d1"], order.by = mon_test[-c(1,2)])) %>% 
  ts.plot(main = "VAR(1) and VAR(2) Prediction in 2019-2023 (Test)",
          col = c("red", "blue", "green"), lwd = 2,
          xlab = "Year", ylab = "value")
legend("bottom", legend = c( "Actual", "VAR(1)", "VAR(2)"), col = c("red", "blue", "green"), 
       xpd = FALSE, lwd = 2, horiz = TRUE, cex = 0.5)

stats::lag(crude_oil_price_WTI_mon[mon_test], k = -1) + 
  zoo(pred1[, "crude_oil_price_WTI_mon_d1"], order.by = mon_test)

rmse(crude_oil_price_WTI_mon_d1[mon_test], 
     zoo(pred1[, "crude_oil_price_WTI_mon_d1"], order.by = mon_test[-1]))
rmse(crude_oil_price_WTI_mon_d1[mon_test], 
     zoo(pred2[, "crude_oil_price_WTI_mon_d1"], order.by = mon_test))

sd(crude_oil_price_WTI_mon_d1[mon_test])

# ARIMA ####

auto.arima(crude_oil_price_WTI_mon[mon_train])
arima012 <- Arima(crude_oil_price_WTI_mon[mon_train], order = c(0, 1, 2))

sqrt(arima012$sigma2)

forecast(arima012) %>% autoplot
predict(arima012, newdata = crude_oil_price_WTI_mon[mon_test])

cbind(crude_oil_price_WTI_mon[mon_test],
      Arima(crude_oil_price_WTI_mon[mon_test], model = arima012)[["fitted"]]) %>% 
  ts.plot(col = c("red", "blue"), lwd = 2, xlab = "year", ylab = "price",
          main = "ARIMA(0,1,2) model Prediction in 2020-2023 (Test)")
legend("bottom", legend = c("Actual", "ARIMA"), col = c("red", "blue"),
       horiz = TRUE, lwd = 2, cex = 0.5)

rmse(crude_oil_price_WTI_mon[mon_test], 
     Arima(crude_oil_price_WTI_mon[mon_test], model = arima012)[["fitted"]])


# ARIMA daily ####

auto.arima(crude_oil_price_WTI[date_train])
arima010 <- Arima(crude_oil_price_WTI[date_train], order = c(0, 1, 0))

sqrt(arima010$sigma2)

forecast(arima010) %>% autoplot
predict(arima010, newdata = crude_oil_price_WTI[date_test])

cbind(crude_oil_price_WTI[date_test],
      Arima(crude_oil_price_WTI[date_test], model = arima010)[["fitted"]]) %>% 
  ts.plot(col = c("red", "blue"), lwd = 2, xlab = "year", ylab = "price",
          main = "ARIMA(0,1,0) Model Daily Prediction in 2020-2023 (Test)", type = "l")
legend("bottom", legend = c("Actual", "ARIMA"), col = c("red", "blue"),
       horiz = TRUE, lwd = 2, cex = 0.5)

rmse(crude_oil_price_WTI[date_test], 
    zoo(Arima(crude_oil_price_WTI[date_test], model = arima010)[["fitted"]],
        order.by = date_test))

# ARIMAX ####

xreg_arimax <- cbind(crude_oil_production_US_d1, 
                     utility_rate, 
                     stocks_US_mon_hpfilter_cycle, 
                     petroleum_dsd2)

auto.arima(crude_oil_price_WTI_mon[mon_train],
           xreg = xreg_arimax[mon_train])

arimax212102 <- Arima(crude_oil_price_WTI_mon[mon_train],
                      order = c(2, 1, 2), seasonal = c(1, 0, 2),
                      xreg = xreg_arimax[mon_train])

sqrt(arimax212102$sigma2)

cbind(crude_oil_price_WTI_mon[mon_test],
      Arima(crude_oil_price_WTI_mon[mon_test],
            xreg = xreg_arimax[mon_test], model = arimax212102)[["fitted"]]) %>% 
  ts.plot(col = c("red", "blue"), lwd = 2, xlab = "year", ylab = "price",
          main = "ARIMAX(2,1,2)(1,0,2) Model Prediction in 2020-2023 (Test)")
legend("bottom", legend = c("Actual", "ARIMAX"), col = c("red", "blue"),
       horiz = TRUE, lwd = 2, cex = 0.5)

rmse(crude_oil_price_WTI_mon[mon_test],
     Arima(crude_oil_price_WTI_mon[mon_test],
            xreg = xreg_arimax[mon_test], model = arimax212102)[["fitted"]])

# ARIMAX, Growth Rate ####

auto.arima(crude_oil_price_WTI_mon_gr[mon_train],stationary = TRUE,
           xreg = x[mon_train])

# LSTM monthly ####

atrain <- crude_oil_price_WTI_mon[mon_train]
spread <- max(atrain) - min(atrain)
atrain <- (atrain - min(atrain)) / spread

#数据拆分为X和Y
create_dataset <- function(set,look_back) { 
  #set表示数据集，look_back表示回溯多少步
  l <- length(set)
  
  dataX <- array(dim = c(l - look_back, look_back))
  for (i in 1:look_back){
    dataX[, i] <- set[i:(l - look_back + i - 1)]}
  
  dataY <- array(data = set[(look_back + 1):l],
                 dim = c(l - look_back, 1)) #label只有1个
  
  return(list(dataX = dataX, dataY = dataY))
}

look_back <- 24
atrainXY <- create_dataset(atrain, look_back)

# reshape input to be [samples, time steps, features] 
dim(atrainXY$dataX) <- c(dim(atrainXY$dataX)[1], 1, dim(atrainXY$dataX)[2])
dim(atrainXY$dataY) <- c(dim(atrainXY$dataY)[1], 1, dim(atrainXY$dataY)[2])

use_python("C:/Users/PC622R/AppData/Local/Programs/Python/Python311/python.exe")
model <- keras_model_sequential()

model %>%
  # 定义模型
  layer_lstm(
    units = 100, #经过lstm层后，形状(a,b,c)会变成(a,b,100)
    input_shape = c(1, look_back)
  ) %>%
  layer_dense(
    units = 20 #经过dense层后，形状(a,b,100)会变成(a,b,20) 
  ) %>%
  
  # 设置优化项
  compile(
    loss = "mean_squared_error", 
    optimizer = "adam" 
  ) %>%
  
  # 运行训练
  fit( 
    atrainXY$dataX,
    atrainXY$dataY,
    epochs = 20, #迭代次数
    batch_size = 36, #每次训练抓取的样本数量
    verbose = 2 #为每个epoch输出一行记录
  )

atrainScore <- model %>%
  evaluate(
    atrainXY$dataX,
    atrainXY$dataY,
    verbose = 2
  )

sprintf(
  "Train Score: %.4f MSE (%.4f RMSE)",
  atrainScore * spread^2,
  sqrt(atrainScore) * spread
)

atest <- crude_oil_price_WTI_mon[c(mon_train[120:length(mon_train)-1], mon_test)]
spread <- max(atest) - min(atest)
atest <- (atest - min(atest)) / spread
atestXY <- create_dataset(atest, look_back)

atestXY$dataX[1, ] <- atrain[(length(atrain)-look_back+1):length(atrain)]
# atestXY$dataX <- array_reshape(atestXY$dataX,
#                          dim = c(dim(atestXY$dataX)[1], 1, dim(atestXY$dataX)[2]))

atestPredict <- vector() # 用于存储预测值

#atestXY$dataX <- array(atestXY$dataX, dim = c(nrow(atestXY$dataX), look_back, ncol(atestXY$dataX)))

for (i in 1:length(atest)){
  # 预测
  predict <- model %>% predict(array(atestXY$dataX, dim = c(nrow(atestXY$dataX), 1, 60)), verbose = 2) 
  # 保存预测值
  atestPredict <- cbind(atestPredict, predict[20])
  # 将已有的预测值作为下一次预测的输入值
  if (i <= look_back){
    atestXY$dataX[i+1, (look_back-i+1):look_back] <- atestPredict 
  }
  if (i > look_back){
    atestXY$dataX[i+1, ] <- atestPredict[(length(atestPredict)-look_back+1):length(atestPredict)]  
  }
}

atestPredict<- predict[,20] * spread + min(atest)
atest <- atest * spread + min(atest)

df <- data.frame(index = 1:length(atest),
                 value = atest,
                 type = "raw") %>%
  rbind(data.frame(index = 61:length(atest),
                   value = atestPredict,
                   type = "atest"))

ggplot(data = df) +
  geom_line(mapping = aes(x = index, y = value, color = type)) +
  geom_point(mapping = aes(x = index, y = value, color = type)) +
  ggtitle("LSTM Model Prediction in 2016-2023 (monthly)")

rmse(atest,
     atestPredict)

# LSTM, Xs ####

x_train <- x[mon_train] %>% 
  na.fill(fill = 0)

l <- 4
m_train <- input_lag(l = l, m=x_train)

y_train <- as.matrix(coredata(crude_oil_price_WTI_mon_d1[mon_train])) %>% 
  .[-c(1:l),] %>% 
  matrix(ncol = 1)

dim(m_train) <- c(dim(m_train)[1], 1, dim(m_train)[2])
dim(y_train) <- c(dim(y_train)[1], 1, dim(y_train)[2])



use_python("C:/Users/PC622R/AppData/Local/Programs/Python/Python311/python.exe")
model <- keras_model_sequential()

model %>%
  layer_lstm(
    units = 24,
    input_shape = c(1, dim(m_train)[3])
  ) %>%
  layer_dense(
    units = 12
  ) %>%
  compile(
    loss = "mean_squared_error", 
    optimizer = "adam" 
  ) %>%
  fit( 
    m_train,
    y_train,
    epochs = 100,
    batch_size = 12,
    verbose = 2
  )

atrainScore <- model %>%
  evaluate(
    m_train,
    y_train,
    verbose = 2
  )

y_predict <- apply(predict(model, m_train), 1, mean) %>% 
  zoo(order.by = mon_train[(l+1):length(mon_train)])

cbind(
  y_predict,
  crude_oil_price_WTI_mon_d1[mon_train]
) %>% 
  ts.plot(col = c("red", "blue"),
          main = "LSTM Model Prediction with lag 6")

x_test <- x[mon_test] %>% 
  na.fill(fill = 0)

m_test <- input_lag(m = x_test, lag = l)
dim(m_test) <- c(dim(m_test)[1], 1, dim(m_test)[2])

y_test_predict <- apply(predict(model, m_test), 1, mean) %>% 
  zoo(order.by = mon_test[(l+1):length(mon_test)])

cbind(y_test_predict,
      crude_oil_price_WTI_mon_d1[mon_test]) %>% 
  ts.plot(col = c("red", "blue"),
          main = "LSTM Model Prediction with lag 6") +
  abline(h=0, lty = 2)

# LSTM daily ####

atrain <- na.fill(crude_oil_price_WTI[date_train], fill = "extend")
spread <- max(atrain) - min(atrain)
atrain <- (atrain - min(atrain)) / spread

look_back <- 60
atrainXY <- create_dataset(atrain, look_back)

dim(atrainXY$dataX) <- c(dim(atrainXY$dataX)[1], 1, dim(atrainXY$dataX)[2])
dim(atrainXY$dataY) <- c(dim(atrainXY$dataY)[1], 1, dim(atrainXY$dataY)[2])

use_python("C:/Users/PC622R/AppData/Local/Programs/Python/Python311/python.exe")
model <- keras_model_sequential()

model %>% 
  layer_lstm(
    units = 100,
    input_shape = c(1, look_back)
  ) %>% 
  layer_dense(
    unit = 20
  ) %>% 
  compile(
    loss = "mean_squared_error",
    optimizer = "adam"
  ) %>% 
  fit(
    atrainXY$dataX,
    atrainXY$dataY,
    epochs = 365,
    batch_size = 30,
    verbose = 2
  )

atrainScore <- model %>% 
  evaluate(
    atrainXY$dataX,
    atrainXY$dataY,
    verbose = 2
  )

sprintf(
  "Training Score: %.4f (%.4f RMSE)",
  atrainScore * spread^2,
  sqrt(atrainScore) * spread
)

atest <- crude_oil_price_WTI[date_test]
atest <- na.fill(atest, fill = "extend")

spread <- max(atest) - min(atest)
atest <- (atest - min(atest)) / spread

look_back <- 60
atestXY <- create_dataset(atest, look_back)

# atestScore <- model %>% 
#   evaluate(
#     atrainXY$dataX,
#     atrainXY$dataY,
#     verbose = 2
#   )
# 
# sprintf(
#   "Test Score: %.4f MSE (%.4f RMSE)",
#   atestScore * spread^2,
#   sqrt(atestScore) * spread
# )

atestXY$dataX <- array(atestXY$dataX,
                       dim = c(dim(atestXY$dataX)[1], 1, dim(atestXY$dataX)[2]))
atestXY$dataY <- array(atestXY$dataY,
                       dim = c(dim(atestXY$dataY)[1], 1, dim(atestXY$dataY)[2]))

atestXY$dataP <- predict(model, atestXY$dataX, verbose = 2)

atestScore <- rmse(atestXY$dataY * spread + min(atest),
                   atestXY$dataP[,20] * spread + min(atest))
sprintf("LSTM model (daily) Test RMSE: %.4f",
        atestScore)

cbind(zoo(atestXY$dataY * spread + min(atest), 
          order.by = date_test),
         zoo(atestXY$dataP[,20] * spread + min(atest),
             order.by = date_test)) %>% 
  ts.plot(col = c("red", "blue"), main = "LSTM Model Prediction in 2020-2023 (daily)")

## LSTM daily 2 ####
# lstm time series prediction in R
# <http://datasideoflife.com/?p=1171>

scale_factors <- c(mean(crude_oil_price_WTI[date_train]),
                   sd(crude_oil_price_WTI[date_train]))

scaled_train <- (crude_oil_price_WTI[date_train]-scale_factors[1]) /
  scale_factors[2]

prediction <- 12
lag <- 12

scaled_train <- as.matrix(scaled_train)

x_train_data <- t(sapply(1:(length(scaled_train)-lag-prediction+1),
       function(x) scaled_train[x:(x+lag-1),1]))

x_train_arr <- array(
  data = as.numeric(unlist(x_train_data)),
  dim = c(nrow(x_train_data), lag, 1)
)

y_train_data <- t(sapply((1+lag):(length(scaled_train)-prediction+1),
                         function(x) scaled_train[x:(x+prediction-1)]))
y_train_arr <- array(
  data = as.numeric(unlist(y_train_data)),
  dim = c(nrow(y_train_data), prediction, 1)
)

lstm_model <- keras_model_sequential()

lstm_model %>%
  layer_lstm(units = 50, # size of the layer
             batch_input_shape = c(1,12,1), # batch size, timesteps, features
             return_sequences = TRUE,
             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

lstm_model %>%
  compile(loss = 'mae', optimizer = 'adam', metrics = 'accuracy')

summary(lstm_model)

lstm_model %>% fit(
  x = x_train_arr,
  y = y_train_arr,
  batch_size = 1,
  epochs = 20,
  verbose = 0,
  shuffle = FALSE
)

lstm_forecast <- lstm_model %>%
  predict(x_train_arr, batch_size = 1) %>%
  .[, , 1]


# we need to rescale the data to restore the original values
lstm_forecast <- lstm_forecast * scale_factors[2] + scale_factors[1]

ts.plot(ts(as.numeric(lstm_forecast[,1])), ts(as.numeric(y_train_arr[,,1][,1])), col = c("blue", "red"),
        main = "LSTM Model Prediction Series Plot in Train Period") +
  legend("bottom", legend = c("Actual", "LSTM"), col = c("red", "blue"),
         horiz = TRUE, lwd = 2, cex = 0.5)



## GARCH-MIDAS ####
# generalized autoregression conditional heteroskedasticity - mixed data sampling

df_garch <- data.table(
  date = date(crude_oil_price_WTI[date_train]),
  crude_oil_price_WTI = coredata(na.fill(crude_oil_price_WTI[date_train], fill = "extend"))
) %>% merge(
  data.table(date = date(gepu[date_train]), 
             gepu = gepu[date_train]),
  by = "date", all = TRUE
) %>% mutate(
  gepu = coredata(nafill(gepu, type = "locf")),
  crude_oil_price_WTI = nafill(crude_oil_price_WTI, type = "nocb"),
  crude_oil_price_WTI_logreturn = c(0, diff(log(crude_oil_price_WTI))),
  month = floor_date(date, unit = "month")
) %>% filter(
  date > as.Date("2005-01-03")
)

md_garch_midas <- fit_mfgarch(data = df_garch, y = "crude_oil_price_WTI", x = "gepu",
            low.freq = "month", K = 60)

sqrt(sum((md_garch_midas$df.fitted[,"residuals"])^2, na.rm = TRUE) / nrow(df_garch))

md_garch_midas$bic

fit_mfgarch(data = df_garch, y = "crude_oil_price_WTI_logreturn", x = "gepu",
            low.freq = "month", K = 60)

rmse(coredata(na.fill(crude_oil_price_WTI[date_test], fill = "extend")),
     predict(md_garch_midas, 
             coredata(na.fill(crude_oil_price_WTI[date_test], fill = "extend"))))

zoo(predict(md_garch_midas, 
        coredata(na.fill(crude_oil_price_WTI[date_test], fill = "extend"))),
    order.by = date(crude_oil_price_WTI[date_test])) %>% 
  cbind(crude_oil_price_WTI[date_test]) %>% 
  ts.plot(main = "GARCH-MIDAS Model Prediction in 2020-2023 (test)",
          col = c("blue", "red")) +
  legend("bottom", legend = c("Actual", "GARCH"), col = c("red", "blue"),
         horiz = TRUE, lwd = 2, cex = 0.5)
# multi_step_ahead_pred(md_garch_midas, h = 10, X = crude_oil_price_WTI[mon_test])

## GARCH ####

crude_oil_price_res <- Return.calculate(crude_oil_price_WTI[date_train], 
                                        method = "log")[-1] %>% 
  na.fill(fill = "extend")
crude_oil_price_res_test <- Return.calculate(crude_oil_price_WTI[date_test], 
                                        method = "log")[-1] %>% 
  na.fill(fill = "extend")

plot.ts(crude_oil_price_res, ylab = "value",
        main = "Return Rate of Crude Oil Series Plot")

auto.arima(crude_oil_price_res, stationary = TRUE)

acf(crude_oil_price_res, na.action = na.pass, lag.max = 60)
pacf(crude_oil_price_res, na.action = na.pass, lag.max = 60)

adf.test(na.omit(crude_oil_price_res))
urca::ur.df(crude_oil_price_res, type = "drift", selectlags = "AIC") %>% 
  summary()

dataToplot <- cbind(crude_oil_price_res, crude_oil_price_res^2,
                    abs(crude_oil_price_res))

plot.zoo(dataToplot, main = "WTI Crude Oil Price Daily Return", 
         ylab = c("X", "X^2", "|X|"), xlab = "Year")

garch11 <- garchFit(formula = ~ 1 + garch(1,1),
                    data = crude_oil_price_res, trace = FALSE)
summary(garch11)

zoo(volatility(garch11), order.by = index(crude_oil_price_res)) %>% 
  plot.zoo(xlab = "Year", ylab = "Volatility",
           main = "WTI Crude Oil Daily Price Volatility Series")
abline(h = sd(crude_oil_price_res), col = "red")

residuals(garch11) %>% 
  zoo(order.by = index(crude_oil_price_res)) %>% 
  plot.zoo(main = "Standarized Residuals of GARCH(1,1)", 
           xlab = "Year")

plot(garch11, which = 10)
plot(garch11, which = 11)
plot(garch11, which = 3)

# which = 1:13
# 1. Time SeriesPlot
# 2. Conditional Standard Deviation Plot
# 3. Series Plot with 2 Conditional SD Superimposed
# 4. Autocorrelation function Plot of Observations
# 5. Autocorrelation function Plot of Squared Observations
# 6. Cross Correlation Plot
# 7. Residuals Plot
# 8. Conditional Standard Deviations Plot
# 9. Standardized Residuals Plot
# 10.ACF Plot of Standardized Residuals
# 11.ACF Plot of Squared Standardized Residuals
# 12.Cross Correlation Plot between $r^2$ and r
# 13.Quantile-Quantile Plot of Standardized Residuals

fitted <- garchFit(spec = garch11, data = crude_oil_price_res)
twelve_step_predict <- predict(garch11, n.ahead = 12, plot = TRUE)

garchFit(spec = garch11, data = crude_oil_price_res_test)

# ru-GARCH ####

spec <- ugarchspec(variance.model = list(garchOrder=c(1,1)),
                     mean.model = list(armaOrder=c(0,0)))

ugarch11 <- ugarchfit(spec = spec, data = crude_oil_price_res)
ugarch11

plot(ugarch11, which = "all")

spec_fixed <- ugarch11
setfixed(spec_fixed) <- as.list(coef(spec_fixed))

ugarchforecast(spec_fixed, data = crude_oil_price_res, n.ahead = 60)

ugarchspec(variance.model = list(garchOrder=c(1,0)),
                        mean.model = list(armaOrder=c(0,0)),
                        fixed.pars=list(mu = 0, omega=0.5, alpha1=0.5)) <- as.list(coef(spec_fixed))

## Wavelet and ANN ####

crude_oil_price_WTI_wavelet <- dwt(ts(crude_oil_price_WTI[date_train]), filter = "la8") 
coef(crude_oil_price_WTI_wavelet, "d")

