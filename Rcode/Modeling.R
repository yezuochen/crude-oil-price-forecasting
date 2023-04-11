#### Crude Oil Price Forecast ####

### Modeling ####

## Packages ####

library(MTS)
library(Metrics)
library(tsDyn)
library(tseries)
library(forecast)
library(keras)
library(reticulate)

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

pred1 <- matrix(ncol = 5, nrow = 0)
for(i in 1:(length(mon_verify))){
  p <- predict(var1_dyn, coredata(var_zoos[mon_verify[i]]), n.ahead = 1) 
  pred1 <- rbind(pred1, p)
}

pred2 <- matrix(ncol = 5, nrow = 0)
for(i in 1:(length(mon_verify)-1)){
  p <- predict(var2_dyn, coredata(var_zoos[mon_verify[c(i, i+1)]]), n.ahead = 1) 
  pred2 <- rbind(pred2, p)
}

ts.plot(cbind(zoo(pred1[, "crude_oil_price_WTI_mon_d1"], order.by = mon_verify[-1]),
              zoo(pred2[, "crude_oil_price_WTI_mon_d1"], order.by = mon_verify[-c(1,2)]),
              crude_oil_price_WTI_mon_d1[mon_verify]),
        col = c("blue", "green", "red"), lwd = 2,
        main = "VAR(1) and VAR(2) Difference Prediction in 2015-2019 (Verifying)",
        xlab = "Year", ylab = "value")
legend("bottom", legend = c("VAR(1)", "VAR(2)", "Actual"), col = c("blue", "green", "red"), 
       xpd = FALSE, lwd = 2, horiz = TRUE, cex = 0.5)

cbind(crude_oil_price_WTI_mon[mon_verify],
      stats::lag(crude_oil_price_WTI_mon[mon_verify], k = -1) + 
        zoo(pred1[, "crude_oil_price_WTI_mon_d1"], order.by = mon_verify[-1]),
      stats::lag(crude_oil_price_WTI_mon[mon_verify], k = -2) + 
        zoo(pred2[, "crude_oil_price_WTI_mon_d1"], order.by = mon_verify[-c(1,2)])) %>% 
  ts.plot(main = "VAR(1) and VAR(2) Prediction in 2015-2019 (Verify)",
          col = c("red", "blue", "green"), lwd = 2,
          xlab = "Year", ylab = "value")
legend("bottom", legend = c( "Actual", "VAR(1)", "VAR(2)"), col = c("red", "blue", "green"), 
       xpd = FALSE, lwd = 2, horiz = TRUE, cex = 0.5)

stats::lag(crude_oil_price_WTI_mon[mon_verify], k = -1) + 
  zoo(pred1[, "crude_oil_price_WTI_mon_d1"], order.by = mon_verify)

rmse(crude_oil_price_WTI_mon_d1[mon_verify], 
     zoo(pred1[, "crude_oil_price_WTI_mon_d1"], order.by = mon_verify[-1]))
rmse(crude_oil_price_WTI_mon_d1[mon_verify], 
     zoo(pred2[, "crude_oil_price_WTI_mon_d1"], order.by = mon_verify))

sd(crude_oil_price_WTI_mon_d1[mon_verify])

# ARIMA ####

auto.arima(crude_oil_price_WTI_mon[mon_train])
arima110 <- Arima(crude_oil_price_WTI_mon[mon_train], order = c(1, 1, 0))


forecast(arima110) %>% autoplot
predict(arima110, newdata = crude_oil_price_WTI_mon[mon_verify])

cbind(crude_oil_price_WTI_mon[mon_verify],
      Arima(crude_oil_price_WTI_mon[mon_verify], model = arima110)[["fitted"]]) %>% 
  ts.plot(col = c("red", "blue"), lwd = 2, xlab = "year", ylab = "price",
          main = "ARIMA(1,1,0) model Prediction in 2015-2019 (Verify)")
legend("bottom", legend = c("Actual", "ARIMA"), col = c("red", "blue"),
       horiz = TRUE, lwd = 2, cex = 0.5)

rmse(crude_oil_price_WTI_mon[mon_verify], 
     Arima(crude_oil_price_WTI_mon[mon_verify], model = arima110)[["fitted"]])

# ARIMAX ####

xreg_arimax <- cbind(crude_oil_production_US_d1, 
                     utility_rate, 
                     stocks_US_mon_hpfilter_cycle, 
                     petroleum_dsd2)

auto.arima(crude_oil_price_WTI_mon[mon_train],
           xreg = xreg_arimax[mon_train])
arimax312001 <- Arima(crude_oil_price_WTI_mon[mon_train],
                      order = c(3, 1, 2), seasonal = c(0, 0, 1),
                      xreg = xreg_arimax[mon_train])

cbind(crude_oil_price_WTI_mon[mon_verify],
      Arima(crude_oil_price_WTI_mon[mon_verify],
            xreg = xreg_arimax[mon_verify], model = arimax312001)[["fitted"]]) %>% 
  ts.plot(col = c("red", "blue"), lwd = 2, xlab = "year", ylab = "price",
          main = "ARIMAX(3,1,2)(0,0,1) Model Prediction in 2015-2019 (Verify)")
legend("bottom", legend = c("Actual", "ARIMAX"), col = c("red", "blue"),
       horiz = TRUE, lwd = 2, cex = 0.5)

rmse(crude_oil_price_WTI_mon[mon_verify],
     Arima(crude_oil_price_WTI_mon[mon_verify],
            xreg = xreg_arimax[mon_verify], model = arimax312001)[["fitted"]])

# LSTM ####

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

look_back <- 60
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

atest <- crude_oil_price_WTI_mon[c(mon_verify, mon_test)]
spread <- max(atest) - min(atest)
atest <- (atest - min(atest)) / spread
atestXY <- create_dataset(atest, look_back)

atestXY$dataX[1, ] <- atrain[(length(atrain)-look_back+1):length(atrain)]
# atestXY$dataX <- array_reshape(atestXY$dataX,
#                          dim = c(dim(atestXY$dataX)[1], 1, dim(atestXY$dataX)[2]))

atestPredict <- vector() # 用于存储预测值

for (i in 1:length(atest)){
  # 预测
  predict <- model %>% predict(atestXY$dataX[i, ], verbose = 2) 
  # 保存预测值
  atestPredict <- cbind(atestPredict, predict)
  # 将已有的预测值作为下一次预测的输入值
  if (i <= look_back){
    atestXY$dataX[i+1, (look_back-i+1):look_back] <- atestPredict 
  }
  if (i > look_back){
    atestXY$dataX[i+1, ] <- atestPredict[(length(atestPredict)-look_back+1):length(atestPredict)]  
  }
}

atestPredict <- atestPredict * spread + min_value

df <- data.frame(index = (train_size + 1):length(arrival$arrival),
                 value = atest,
                 type = "raw") %>%
  rbind(data.frame(index = 1:length(atestPredict) + look_back + length(atrain),
                   value = atestPredict,
                   type = "atest"))

ggplot(data = df) +
  geom_line(mapping = aes(x = indexy = value, color = type)) +
  geom_point(mapping = aes(x = index, y = value, color = type)) +
  geom_vline(xintercept = length(atrain) + 0.5) +
  theme_economist() +
  scale_color_economist() +
  ggtitle("2016-12下旬 每分钟进站人数的预测值与实际值对比")
