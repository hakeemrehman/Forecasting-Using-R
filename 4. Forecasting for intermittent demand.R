# Load the required library
library(tsintermittent)

# Load the 'ts3' time series
y <- ts(scan("ts3.txt"), start=c(2011,1), frequency=12)
y.test <- ts(scan("ts3out.txt"), start=c(2016,1), frequency=12)

# Set the forecat horizon to be equal to the test set
h <- length(y.test)
h

# Plot the series to get a general impression
plot(y)

# Data points
y

# Decomposition of a time series
crost.decomp(y)

# Croston's method
'******************'
f.crost <- crost(y,h=h,outplot=TRUE)
f.crost

f.crost$weights # Values of Alphas
f.crost$model # Gives name of model applied
f.crost$components # non-zero demand & inter-demand interval components
f.crost$frc.in # In-sample forcast (fitted) values (Demand/Interval)
f.crost$frc.out # out-of-sample forecast (Demand/Interval)


# Syntetos-Boylan approximation (SBA)
'************************************'
f.sba <- crost(y,h=h,type="sba")
f.sba
f.sba$frc.out # out-of-sample forecast (Demand/Interval)

# Shale-Boylan-Johnston correction
'*********************************'
f.sbjc <- crost(y,h=h,type="sbj")
f.sbjc
f.sbjc$frc.out # out-of-sample forecast (Demand/Interval)


# Teunter, Syntetos and Babai (TSB) 
'*********************************'
f.tsb <- tsb(y,h=h)
f.tsb
f.tsb$frc.out

'Forecasting intermittent time series with Temporal Aggregation
---------------------------------------------------------------'
# Multiple Aggregation Prediction Algorithm (MAPA)
'*************************************************'
f.imapa <- imapa(y,h=h)
f.imapa
f.imapa$frc.out

# Aggregate–Disaggregate Intermittent Demand Approach (ADIDA) method
'*******************************************************************'
'Although IMAPA uses several temporal aggregation levels, if we restrict 
 it to a single one we can get ADIDA based forecasts'
f.adida <- as.vector(imapa(y,h=h,minimumAL=h+1,maximumAL=h+1))
f.adida
f.adida$frc.out

# Measures of Forecast Accuracy
'------------------------------'
# How to best evaluate forecasts of intermittent demand is an active research are.
# Here we will use ME and RMSE
f.all <- cbind(f.crost$frc.out,f.sba$frc.out,f.sbjc$frc.out,f.tsb$frc.out,
               f.imapa$frc.out)
colnames(f.all) <- c("Croston","SBA","SBJ","TSB","MAPA")
k <- dim(f.all)[2]
y.all <- t(tcrossprod(rep(1,k),y.test))
E <- y.all - f.all
ME <- apply(E,2,mean)
RMSE <- sqrt(apply(E^2,2,mean))
res <- rbind(ME,RMSE)
print(res)




