# Load the necessary libraries
library(forecast)
library(greybox)

# Load two time series, as before
ts2 <- ts(scan("ts2.txt"), start=c(2011,1), frequency=12)
plot(ts2)

# Load some test data; In order to evaluate the forecasts 
ts2.test <- ts(scan("ts2out.txt"), start=c(2016,1), frequency=12)

# Model the 'ts2'
y <- ts2
y.test <- ts2.test

# Set forecast horizon
h <- length(y.test)
h

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

# Measures of Forecast Accuracy using accuracy() function
'--------------------------------------------------------'
accuracy(f.arima) # Gives only in-sample accuracy
accuracy(f.arima,y.test) # Gives in-sample & out-of-sample accuracy
accuracy(f.arima$mean,y.test) # Gives out-of-sample accuracy

# 1. In-Sample Forecast Accuracy
'*******************************'
AUTO_ETS <- accuracy(f.ets)
AUTO_ARIMA <- accuracy(f.arima)

f.all <-rbind(AUTO_ETS, AUTO_ARIMA)
rownames(f.all) <- c("Auto ETS Model","Auto ARIMA Model")
f.all

# 2. Out-of-Sample Forecast Accuracy
'***********************************'
AUTO_ETS <- accuracy(f.ets$mean,y.test)
AUTO_ARIMA <- accuracy(f.arima$mean,y.test)

f.all <-rbind(AUTO_ETS, AUTO_ARIMA)
rownames(f.all) <- c("Auto ETS Model","Auto ARIMA Model")
f.all

# Measures of Forecast Accuracy using measures() function
'--------------------------------------------------------'
'measures(holdout, forecast, actual, digits = NULL, 
  benchmark = c("naive","mean"))'
options(scipen = 999)

# 1. In-Sample Forecast Accuracy
'*******************************'
AUTO_ETS1 <- measures(y,f.ets$fitted,y)
AUTO_ARIMA1 <- measures(y,f.arima$fitted,y)

f.all1 <-rbind(AUTO_ETS1, AUTO_ARIMA1)
rownames(f.all1) <- c("Auto ETS Model","Auto ARIMA Model")
f.all1

# 2. Out-of-Sample Forecast Accuracy
'***********************************'
AUTO_ETS1 <- measures(y.test,f.ets$mean,y)
AUTO_ARIMA1 <- measures(y.test,f.arima$mean,y)

f.all1 <-rbind(AUTO_ETS1, AUTO_ARIMA1)
rownames(f.all1) <- c("Auto ETS Model","Auto ARIMA Model")
f.all1

# 3. Rolling Horizon Forecast Accuracy
'*************************************'
# Combine the in-sample & holdout datasets
data <- ts(c(y,y.test), start=c(2011,1),frequency = 12)
data

# Number of observations 
obs <- 72

# Forecast horizon & define Number of Origins
h <- 3
origins <- 9

datavalues <- setNames(vector("list",3),
                            c("actuals","holdout","mean"))
datavalues$actuals <- data
datavalues$holdout <-
  datavalues$mean <- matrix(NA,h,origins,
                            dimnames=list(paste0("h",1:h),
                                  paste0("origin",1:origins)))

for(i in 1:origins){
  # Fit the model
  testModel <- ets(data[1:(obs+i-origins-h)])
  # Drop the in-sample observations and extract the first h observations from the rest
  datavalues$holdout[,i] <- head(data[-c(1:(obs-origins+i-h))], h)
  # Produce forecasts and write down the mean values
  datavalues$mean[,i] <- forecast(testModel, h=h)$mean
}

# Get the test dat (holdout)
datavalues$holdout

# Get the forecasted values
datavalues$mean

# Measures of Forecast Accuracy
'------------------------------'
# Calculate Errors
errors <- datavalues$holdout-datavalues$mean
errors

# Forecast errors Visualization
boxplot(errors)

ME <- apply(errors,2,mean)
RMSE <- sqrt(apply(errors^2,2,mean))
MAE <- apply(abs(errors),2,mean)
res <- rbind(ME,RMSE,MAE)
print(res)
















