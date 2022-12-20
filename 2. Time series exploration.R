' Forecasting with R 
--------------------'

'Time series exploration 
************************'

# Install the necessary libraries
'********************************'
install.packages("forecast")
install.packages("tsutils")

# Load the necessary libraries
'*****************************'
library(forecast)
library(tsutils)

# Load the three time series
'***************************'
ts1 <- ts(scan("ts1.txt"), start=c(2011,1), frequency=4)
ts2 <- ts(scan("ts2.txt"), start=c(2011,1), frequency=12)
ts3 <- ts(scan("ts3.txt"), start=c(2011,1), frequency=12)

# Store the time series in to a variable `y' 
y <- ts2

# Plot the series to get a general impression
plot(y)

# ----- Trend Analysis -----
'****************************'
# Look for trend in the data, by calculating the Centred Moving Average
cma <- cmav(y, ma=3, outplot=1) # 3 monthly moving average
cma <- cmav(y, ma=15, outplot=1) # 15 monthly moving average
cma <- cmav(y, outplot=1) 

# Result shows upward trend in the time series
print(cma)

'The function cmav by default backcasts and forecasts the missing starting 
and ending values using ETS.If we want to stop this we can use the following'
cmav(y, outplot=1, fill=FALSE)

# Is the trend significant? 
# Cox-Stuart test --- non-parametric test
coxstuart(cma)

# ----- Seasonality Analysis -----
'*********************************'
# Test for seasonality visually by producing a seasonal plot
seasplot(y) # it will removes the trend automatically
seasplot(y,outplot=2)
seasplot(y,outplot=3)
seasplot(y,outplot=4)

s <- seasplot(y)
s$season           # It provides the estimated Seasonal indices
s$season.exist
s$season.pval
s$trend           # Trend (if any)
s$trend.exist
s$trend.pval
s$decomposition

# The equivalent function in the forecast package is:
seasonplot(y)
'The main differences are in the additional options that seasplot offers 
such as estimation of seasonal indices and visualisations'



