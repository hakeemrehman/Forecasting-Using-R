' Forecasting with R 
--------------------'

'Forecasting for fast demand 
****************************'
# Load the necessary libraries
library(forecast)
library(MAPA)
library(tsutils)

# Load two time series, as before
ts1 <- ts(scan("ts1.txt"), start=c(2011,1), frequency=4)
ts2 <- ts(scan("ts2.txt"), start=c(2011,1), frequency=12)
plot(ts1)
plot(ts2)

# Load some test data; In order to evaluate the forecasts 
ts1.test <- ts(scan("ts1out.txt"), start=c(2016,1), frequency=4)
ts2.test <- ts(scan("ts2out.txt"), start=c(2016,1), frequency=12)

# Model the 'ts2'
y <- ts2
y.test <- ts2.test

# Set forecast horizon
h <- length(y.test)
h

'Some Simple Forecasting Methods
********************************'
# 1. Mean Method
'***************'
# Mean value h times
f.mean <- meanf(y,h)
f.mean
f.mean$fitted # in-sample forecast value
plot(f.mean) # forecast with prediction interval
f.mean$mean

# Convert into ts object for plotting the series & forecast together. 
f.mean <- ts(f.mean$mean,start=start(y.test),frequency=frequency(y.test))
# Now we can plot all elements
ts.plot(y,y.test,f.mean,col=c("blue","blue","red"))

# 2. Naive Method
'****************'
# Replicate the last value h times
f.naive <- rep(tail(y,1),h)
f.naive

'or use the following functions'
f.naive <-naive(y,h=h) 
f.naive
f.naive$fitted
rwf(y,h=h)

# Convert into ts object for plotting the series & forecast together. 
f.naive <- ts(f.naive$mean,start=start(y.test),frequency=frequency(y.test))
# Now we can plot all elements
ts.plot(y,y.test,f.naive,col=c("blue","blue","red"))


# 3. Seasonal Naive Method
'*************************'
f.snaive <- rep(tail(y,frequency(y)),ceiling(frequency(y)/h))
f.snaive
# or use the following fuction
f.snaive <- snaive(y,h=h)
f.snaive
f.snaive$fitted

f.snaive <- ts(f.snaive$mean,start=start(y.test),frequency=frequency(y.test))
ts.plot(y,y.test,f.snaive,col=c("blue","blue","red"))

# 4. Drift Method
'****************'
f.drift <- rwf(y,drift=TRUE,h=h)
f.drift
f.drift$fitted
f.drift <- ts(f.drift$mean,start=start(y.test),frequency=frequency(y.test))
ts.plot(y,y.test,f.drift,col=c("blue","blue","red"))

'Exponential Smoothing Methods
******************************'
# ses() -- Simple Exponential Smoothing Model (N,N)
'**************************************************'
f.ses <- ses(y,h=h)
f.ses
f.ses$model
f.ses$mean
f.ses$fitted
f.ses <- ts(f.ses$mean,start=start(y.test),frequency=frequency(y.test))
ts.plot(y,y.test,f.ses,col=c("blue","blue","red"))

# holt() -- (A,N) -- Holt’s linear Trend method
'**********************************************'
f.holtlinear <- holt(y,h=h,damped=FALSE,exponential = FALSE,type="multiplicative") # damped=TRUE for damped trend
f.holtlinear
f.holtlinear$model
f.holtlinear <- ts(f.holtlinear$mean,start=start(y.test),frequency=frequency(y.test))
ts.plot(y,y.test,f.holtlinear,col=c("blue","blue","red"))

# hw() -- (A,M) -- Multiplicative Holt-Winters’ method
'*****************************************************'
f.hw <- hw(y, h=h,seasonal = "multiplicative") 
f.hw
f.hw <- ts(f.hw$mean,start=start(y.test),frequency=frequency(y.test))
ts.plot(y,y.test,f.hw,col=c("blue","blue","red"))

# ets() -- Automatically select Exponential Smoothing Model
'**********************************************************'
fit.ets <- ets(y)

# Print the details of the selected model
fit.ets 

# Forecasting 
f.ets <- forecast(fit.ets,h=h)
f.ets

# Plot the resulting forecast and prediction intervals
plot(f.ets)

# Add the true test values to the plot
lines(y.test,col="red")

# Note that we can easily force a specific model 
ets(y,model="AAA") # Additive errors, Additive trend, Additive seasonality

# ARIMA Models
'*************'

# AR(1) Model
'************'
fit.ar1 <- Arima(y,c(1,0,0))
fit.ar1 

# Forecasting 
f.ar1 <- forecast(fit.ar1,h=h)
f.ar1

# Plot the resulting forecast and prediction intervals
plot(f.ar1)

# Add the true test values to the plot
lines(y.test,col="red")

# MA(1) Model
'************'
fit.ma1 <- Arima(y,c(0,0,1))
fit.ma1 

# Forecasting 
f.ma1 <- forecast(fit.ma1,h=h)
f.ma1

# Plot the resulting forecast and prediction intervals
plot(f.ma1)

# Add the true test values to the plot
lines(y.test,col="red")

# auto.arima() -- Automatically select ARIMA Model
'**************************************************'
fit.arima <- auto.arima(y)
fit.arima

# Forecasting
f.arima <- forecast(fit.arima, h=h)
f.arima

# Check the following two commands--- needs correction
plot(f.arima)
lines(y.test,col="red")

# Trigonometric Exponential Smoothing (TBATS) Method
'***************************************************'
fit.tbats <- tbats(y)
f.tbats <- forecast(fit.tbats,h=h)
plot(f.tbats)
lines(y.test,col="red")

# Theta using 'tsutils' package 
'******************************'
fit.theta <- theta(y)
fit.theta
plot(fit.theta)
f.theta <- forecast.theta(fit.theta,h=h)
plot(f.theta)
lines(y.test,col="red")

# Theta Models using forecTheta package 
'**************************************'
library(forecTheta)
fit.stheta <- stheta(y, h=h, s=NULL)
fit.stheta
plot(fit.stheta)
lines(y.test,col="red")

# Optimized Theta model (otm)
'****************************'
fit.otm <- otm(y, level=NULL, h=h, opt.method="Nelder-Mead") 
fit.otm
plot(fit.otm)
lines(y.test,col="red")

# Measures of Forecast Accuracy
'------------------------------'
accuracy(f.arima) # Gives only in-sample accuracy
accuracy(f.arima,y.test) # Gives in-sample & out-of-sample accuracy
accuracy(f.arima$mean,y.test) # Gives out-of-sample accuracy

# 1. In-Sample Forecast Accuracy
'*******************************'
MEAN <- accuracy(f.mean)
NAIVE <- accuracy(f.naive)
SNAIVE <- accuracy(f.snaive)
DRIFT <- accuracy(f.drift)
SES <- accuracy(f.ses)
HOLTLINEAR <- accuracy(f.holtlinear)
HOLTWINTER_M <- accuracy(f.hw)
AUTO_ETS <- accuracy(f.ets)
AUTO_ARIMA <- accuracy(f.arima)
TBATS <- accuracy(f.tbats)
THETA <- accuracy(f.theta)

f.all <-rbind(MEAN, NAIVE, SNAIVE, DRIFT, SES, HOLTLINEAR, HOLTWINTER_M, 
              AUTO_ETS, AUTO_ARIMA, TBATS, THETA)
rownames(f.all) <- c("MEAN","NAIVE","SNAIVE","DRIFT","SES","HOLT LINEAR",
                     "Multiplicative Holt-Winters","Auto ETS Model",
                     "Auto ARIMA Model", "TBATS", "Theta Method")
f.all

# 2. Out-of-Sample Forecast Accuracy
'***********************************'
MEAN <- accuracy(f.mean$mean,y.test)
NAIVE <- accuracy(f.naive$mean,y.test)
SNAIVE <- accuracy(f.snaive$mean,y.test)
DRIFT <- accuracy(f.drift$mean,y.test)
SES <- accuracy(f.ses$mean,y.test)
HOLTLINEAR <- accuracy(f.holtlinear$mean,y.test)
HOLTWINTER_M <- accuracy(f.hw$mean,y.test)
AUTO_ETS <- accuracy(f.ets$mean,y.test)
AUTO_ARIMA <- accuracy(f.arima$mean,y.test)
TBATS <- accuracy(f.tbats$mean,y.test)
THETA <- accuracy(f.theta$mean,y.test)

f.all <-rbind(MEAN, NAIVE, SNAIVE, DRIFT, SES, HOLTLINEAR, HOLTWINTER_M, 
              AUTO_ETS, AUTO_ARIMA, TBATS, THETA)
rownames(f.all) <- c("MEAN","NAIVE","SNAIVE","DRIFT","SES","HOLT LINEAR",
                     "Multiplicative Holt-Winters","Auto ETS Model",
                     "Auto ARIMA Model", "TBATS", "Theta Method")
f.all

