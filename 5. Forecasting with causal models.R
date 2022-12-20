# Simple Regression
'******************'
# Scan the sales and advertising data
sales <- scan("salesreg.txt")
advertising <- scan("advertising.txt")

# Combine the two set of data in a data frame
data <- data.frame(sales, advertising)

# Plot the data as a scatterplot
plot(advertising, sales, xlab="Advertising", ylab="Sales")

# Calculate the correlation
cor(advertising, sales)

# Fit a simple regression model
fit <- lm(sales ~ advertising, data)
fit

# Show the summary of the fitted model
summary(fit)

# Draw the best fit line
lines(x=advertising, y=fit$fitted.values, col="red")

# Forecasting Using Simple Regression Model
'******************************************'
# Create a new data frame to hold advertising plans
new_data = data.frame(advertising=c(13.8, 15.5, 16.3))

# Predictions based on the fitted model, including the standard error
predict(fit, new_data, se.fit=TRUE)


# Linear regression on trend
'***************************'
# Scan the quarterly sales of the iPhone and transform into a time series
iphone <- ts(scan("iphone.txt"), start=c(2007,2), frequency=4)

# Plot the data
plot(iphone, ylab="Sales", main="iPhone quarterly sales")

# Create a trend indicator (i.e., # of time periods)
t = 1:length(iphone)

# Fit a simple regression model
fit <- lm(iphone ~ t)
# Show the summary of the fitted model
summary(fit)
# Add to the existing graph the fitting values
lines(ts(fit$fitted, start=c(2007,2), frequency=4), col="red", lwd=2)

# Residual diagnostics
'*********************'
# Plot the residuals on time
plot(fit$residuals, ylab="Residuals", xlab="Year")
abline(h=0, col="grey")

# Check for autocorrelation in the residuals
acf(fit$residuals)
'Output: Significant Positive value at Lag=4; indicate data are 
quarterly,this could be an indication of a seasonal patterns.'

# Histogram of residuals
hist(fit$residuals, xlab="Residuals", prob=TRUE)
# Compare with normal distribution
lines(-10000:15000, dnorm(-10000:15000, 0, sd(fit$residuals)), col="red")

# Multiple Regression Model
'**************************'
# Scan data (sales)
sales <- scan("sales.txt") 
sales

# Plot the sales as a line
plot(sales, type="l", xlab="Weeks", ylab="Sales")

# Save the length of the sales into a variable
n <- length(sales)
n

# Scan the promotional data and save into a data frame
promos <- data.frame(promo1=scan("promo1.txt"), promo2=scan("promo2.txt"), 
                     promo3=scan("promo3.txt"))
# Diplay the data frame in the console
promos 

# Save the length of the information regarding forthcoming promotional activity (or forecasting horizon)
h <- nrow(promos)-n 
h

# Plot the promotional activity information on the existing plot
points(which(promos[,1]==1), sales[which(promos[,1]==1)], pch=1, col="blue", cex=1.5, lwd=2)
points(which(promos[,2]==1), sales[which(promos[,2]==1)], pch=2, col="red", cex=1.5)
points(which(promos[,3]==1), sales[which(promos[,3]==1)], pch=3, col="orange", cex=1)

# Fit the Regression Model
fit1 <- lm(sales ~ promo1 + promo2 + promo3, data=promos[1:n,])
fit1

# Get the summary of the model
summary(fit1)

# Add a new line (the fit of the model) on the existing graph
lines(fit1$fit, col="blue") 

# Create lagged variables of the promotions
promos_lag <- rbind(c(0,0,0), promos)
names(promos_lag) <- c("promo1_lag", "promo2_lag", "promo3_lag")

# Combine the two data frames
promos <- cbind(promos, promos_lag[1:(n+h),])

# Diplay the expanded data frame in the console
promos 

# Fit an additive regression model, adding the lagged effects of the promotions
fit2 <- lm(sales ~ promo1 + promo2 + promo3 + promo1_lag + promo2_lag + promo3_lag, promos[1:n,])

# Return the summary of the model
summary(fit2) 

# Add a new line (the fit of the model) on the existing graph
lines(fit2$fit, col="green") 

# Calculate the out-of-sample forecasts, based on the available information on forthcoming promotions
fcs <- predict(fit2, promos[(n+1):(n+h),])
fcs

# Plot the sales as a line
plot(sales, type="l", xlab="Weeks", ylab="Sales", xlim=c(1,109))

# Plot the forecasts as a new line
lines(x=(n+1):(n+h), fcs, col="blue")

# Calculate the natural logarithm of the sales
logsales <- log(sales) 

# Fit a regression model
fitlog <- lm(logsales ~ promo1 + promo2 + promo3 + promo1_lag + promo2_lag + promo3_lag, promos[1:n,])

# Return the summary of the model
summary(fitlog) 

# Plot the sales as a line
plot(sales, type="l", lwd=2)

# Add a line for the fitted values (which are transformed using the exponential function)
lines(exp(fitlog$fit), col="orange")

# Selecting independent variables
'********************************'
# Backwards stepwise regression
step(lm(sales ~ promo1 + promo2 + promo3 + promo1_lag + promo2_lag + promo3_lag, promos[1:n,]))

# Lasso, Ridge & Elastic-Net Regression models
'*********************************************'
# Load the necessary library
library(glmnet)

# Get the data 
sales <- scan("salesreg.txt")
advertising <- scan("advertising.txt")

# Let us load some additional potential explanatory variables 
X <- read.csv("indicators.csv")

# Combine all data sets in a data frame
data <- data.frame(sales, advertising,X)
data

# Check the correlations between the target and the explanatory variables
round(cor(data)[1,],2) # shows the correlation between Sales & others variable

# Fit LASSO regression
'*********************'
fit.Lasso <- glmnet(x=as.matrix(data[,2:23]),y=data[,1],
                    alpha=1, # Lasso Plenty
                    family = "gaussian")
fit.Lasso
# Give us
'Df (the number of nonzero coefficients), 
 %dev (the percent deviance explained), 
 Lambda (the corresponding value of λ). '

# Gives the optimal set of coefficients for each lambda. 
plot(fit.Lasso, xvar='lambda', main="Lasso")

# Select the best lambda 
'***********************'
'One approach for selecting best Lamda is the use cross-validation'
cv.fit.Lasso <- cv.glmnet(x=as.matrix(data[,2:23]),y=data[,1],grouped=FALSE,
                          alpha=1, # Lasso Plenty
                          family = "gaussian")
cv.fit.Lasso
# cvm(mean cross-validated error)
best.lambda.Lasso = cv.fit.Lasso$lambda.min
best.lambda.Lasso

# Get the coefficients of the fitted Model
beta.lasso <- coef(cv.fit.Lasso)
beta.lasso # LASSO able to identify that only advertising is relevant

# Compare Linear Regression with LASSO Solution
'***********************************************'
fit.ols <- lm(sales ~ advertising, data)
beta.ols <- coef(fit.ols)

# Compare the two sets of coefficients
beta.lasso <- as.numeric(beta.lasso)[1:2]
beta.diff <- beta.ols - beta.lasso
print(beta.diff)

# We can see that they are different. This is because the `best' lambda
# is not zero, in which case LASSO would be identical to OLS
fit.cv$lambda.1se

# Finally let us compare the quality of the fit
yhat.fit.cv <- predict(cv.fit.Lasso,as.matrix(data[2:23]))
yhat.fit.ols <- predict(fit.ols,data[2:23])
MSE <- c(mean((data[,1] - yhat.fit.cv)^2), mean((data[,1] - yhat.fit.ols)^2))
names(MSE) <- c("LASSO","OLS")
print(MSE)

# NOTE:
'Observe that the MSE of LASSO is greater than OLS. This is to be expected, 
as LASSO attempts to find the optimal balance between bias and variance to 
avoid overfitting, as controlled by lambda.'

# Plot the target variable and the two model fits
plot(sales,type="o")
lines(yhat.fit.cv,col="red")
lines(yhat.fit.ols,col="blue")
legend("topright",c("Sales","LASSO","OLS"),col=c("black","red","blue"),lty=1)





















