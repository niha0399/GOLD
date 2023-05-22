super<- X1979_2021
str(super)

super$Date <- format(as.POSIXct(super$Date,format='%d-%m-%Y'),format='%b-%Y')

#convert to ts object
volume.ts <- ts(super$Europe, start = c(1990,1), end = c(2000, 12), freq = 12) 
plot(volume.ts, xlab = "Time", ylab = "Volume")

#Adding linear trend line
volume.ts.lm <- tslm(volume.ts ~ trend)
lines(volume.ts.lm$fitted, lwd = 2)

volume.ts.zoom <- window(volume.ts, start = c(1990, 1), end = c(2000, 12))

# 4. Draw a time plot 
plot(volume.ts.zoom, xlab = "Time", ylab = "Volume")

#Partioning the data set 
train.ts <- window(volume.ts, start = c(1990, 1), end = c(1999, 12))
valid.ts <- window(volume.ts, start = c(1999, 1), end = c(2000, 9))

#regression
volume.lm <-  tslm(train.ts ~ trend + I(trend^2))
volume.lm

#forecast in the validation period
volume.lm.pred <- forecast(volume.lm, h = 21, level = 0)
names(volume.lm.pred)
#draw using training-period values and forecasts
plot(volume.ts, ylab = "Volume", xlab = "Time")
lines(volume.lm.pred$fitted, lwd = 2)
lines(volume.lm.pred$mean, lwd = 2, lty=2)
lines(c(1999.25, 1999.25), c(0, 350000000000)) 
lines(c(2000.25, 2000.25), c(0, 3500000000))
text(1998.25, 2230, "Training")
text(1999.75, 2230, "Validation")

accuracy(volume.lm.pred$mean, valid.ts)
#RSME 45.2773

#Naive Forecasting

volume.naive<-naive(train.ts,h=21)
volume.naive
names(volume.naive)

lines(volume.naive$mean,lwd=2,lty=2,col="red")

accuracy(volume.naive$mean,valid.ts)
#RSME 12.61062 

#regression for tainibg period
##train.lm <- tslm(train.ts ~ trend)
#time plot
##plot(train.ts, xlab = "Time", ylab = "Volume", ylim = c(1300, 2300), bty = "l")
#linear trend line
##lines(train.lm$fitted, lwd = 2)
##summary(train.lm)

##Smoothing

#exponential smoothing with alpha=.2 using ANN
ses.pred <- forecast(ses, h = 21, level = 0)
plot(volume.ts,ylab = "Europe", xlab = "Time")
lines(ses.pred$fitted, lwd=2, col="blue")
lines(ses.pred$mean, lwd=2, lty=2, col="blue")
lines(c(2000.0, 2000.0), c(0, 3500)) 
text(1999, 170, "Training")
text(2000.85, 170, "Validation")

#accuracy measures when alpha=.2
accuracy(ses.pred$mean, valid.ts)
#RSME 25.43764

#exponential smoothing without presetting the alpha
#the alpha is chosen automatically
ses.opt <- ets(train.ts, model = "ANN")
ses.opt.pred <- forecast(ses.opt, h = 21, level = 0)

#accuracy measures when the alpha is chosen automatically
accuracy(ses.opt.pred$mean, valid.ts)
#RSME 12.94126

#exponential smoothing with model=MAA
hwin <- ets(train.ts, model = "MAA")
#calculate forecasts
hwin.pred <- forecast(hwin, h = 21, level = 0)
plot(usd.ts,ylab = "usd", xlab = "Time")
lines(hwin.pred$fitted, lwd = 2, col = "blue")
lines(hwin.pred$mean, lwd = 2, lty=2, col = "blue")
lines(c(2000.0, 2000.0), c(0, 3500)) 
text(1999, 170, "Training")
text(2000.85, 170, "Validation")
accuracy(hwin.pred$mean, valid.ts)
#RSME 10.9766

#exponential smoothing with parameters chosen automatically 
ets.opt <- ets(train.ts)
ets.opt.pred <- forecast(ets.opt, h = 21, level = 0)
plot(volume.ts,ylab = "Eurpe", xlab = "Time")
lines(ets.opt.pred$fitted, lwd = 2, col = "blue")
lines(ets.opt.pred$mean, lwd = 2, lty=2, col = "blue")
lines(c(2000.0, 2000.0), c(0, 3500)) 
text(1999.25, 170, "Training")
text(2020.85, 170, "Validation")
accuracy(ets.opt.pred$mean, valid.ts)
#RSME 12.94126


##ARIMA 

#regression model with quadratic trend and additive seasonality terms
train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

#autocorrelation chart for residuals
Acf(train.lm.trend.season$residuals, lag.max = 12, main = "")
Acf(train.lm.trend.season$residuals, lag.max = 12, main = "")$acf

train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
#AR(1) model to residuals
par(mfrow=c(2,1))
Acf(train.lm.trend.season$residuals)
Pacf(train.lm.trend.season$residuals)

train.res.arima <- Arima(train.lm.trend.season$residuals, order = c(1,0,0))
train.res.arima.pred <- forecast(train.res.arima, h = 21)

plot(train.lm.trend.season$residuals, ylab = "Residuals", xlab = "Time")
lines(train.res.arima.pred$fitted, lwd = 2, col = "blue")

summary(train.res.arima)
#RSME 21.27827
