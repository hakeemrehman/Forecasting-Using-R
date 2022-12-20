# Load the requires packages
library(hts)
library(fpp) # contains data sets


# Bottom level data
vn 
# Return the first time series
vn[,1] 

# Convert the data into an hierarchy
y <- hts(vn, nodes=list(4, c(2,2,2,2))) 
# Return the hierarchy
y 
# Return the labels of the nodes at each level
y$labels 
# Return how the nodes are organised
y$nodes 

# Hierarchy wise data  Aggregation 
aggts(y)

# Training Data
data <- window(y, start = c(1998,1), end = c(2009,4))
# Test Data
test <- window(y, start=c(2010,1), end=c(2011,4))

# Bottom-up approach
bu_f <- forecast.gts(data, h=8, method="bu", fmethod="ets")
# Summary of the output
bu_f

# All-levels forecasts
allts(bu_f) 

# To get the forecast at defined level
aggref_bu<-aggts(bu_f, levels = 2) 
aggref_bu 

# Out-of-Sample Measures of Accuracy
'***********************************'
# all levels accuracy measures
accuracy.gts(bu_f,test) 

# Level-0 accuracy measures
accuracy(bu_f, test, levels = 0) 

# write the results
accBU <- accuracy(bu_f, test)
write.csv(accBU,"MOA_BU.csv")  

# Get the out-of-available data forecast & Plot all forecasts
bu_f <- forecast.gts(y, h=8, method="bu", fmethod="ets")
allts(bu_f) 
plot(bu_f) 

# Top-down approach (Average historical proportions)
# for proportion of historical averages, use "tdgsf"
# for forecast proportions, use "tdfp"
allf <- forecast.gts(y, h=4, method="tdgsa", fmethod="ets")
allf
plot(allf) 

# Middle-out approach
allf <- forecast.gts(y, h=4, method="mo", fmethod="ets", level=1)
plot(allf)

# Optimal reconciliation approach
allf <- forecast.gts(y, h=4, method="comb", fmethod="ets")
allf

allf1 <- forecast.gts(y, h=4, method="comb", fmethod="ets",weights ="ols" )
allf1

allf2 <- forecast.gts(y, h=4, method="comb", fmethod="ets",weights ="wls" )
allf2

allf3 <- forecast.gts(y, h=4, method="comb", fmethod="ets",weights ="mint" )
allf3

plot(allf)

