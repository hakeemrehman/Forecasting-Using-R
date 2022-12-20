' Forecasting with R 
--------------------'

'Introduction to R 
*******************'

# ----- R as calculator -----
'*****************************'
# simple calculations 
(9 + 8) / 5 

# Multiplication & power
3*5^2 

# Division
9 / 2 

# Integer division
9 %/% 2 

# Remainder of integer division
9 %% 2 

# Square Root Function
sqrt(9) 

# saving a result into a variable
x <- sqrt(4 * max(-3, 9, 0.8)) 
x

# Boolean variable
y <- TRUE 


# ----- R as programming language -----
'**************************************'
b <- 5
c <- 3

# if Condition
if (b < 4){
  # this will run only if the condition (b < 4) is true
  d <- b+c 
  d
}
# if-else Condition
if (b <= 4){
  # this will run if the condition (b <= 4) is true
  d <- b+c 
} else {
  # this will run if the condition is false
  d <- b-c 
}
d

# For Loop
for (i in 1:10){
  # prints the message "Hello!" ten times
  print("Hello!") 
}

# initialize s with zero
s <- 0 
for (i in 1:20){
  # calculate the following expression
  s <- s + i^3 
  print(s)
  s
}


# ----- Data structures -----
'****************************'

'Vector
-------'
# A vector containing 10 items (elements)
y = c(4,6,2,5,3,7,10,12,4,14) 
y

# Another vector of size 7 that is read from an external file
z = scan("z_vector.txt") 

# Returns the 3nd element of y
y[3] 

# Returns elements 3, 4, 5 and 6 of y
y[3:6] 

# returns the 7th element of z
z[7] 

# Add each element of y with the respective element of z
y + z 

# multiplies each element of y with the respective element of z
y * z 

# Add 3 to each element of y
y + 3 

# Divide each element of y by 2
y / 2 

'Matrix
-------'
# Read data from an external file and arrange as a matrix of order 3x5 "byrow" argument 
m <- matrix(scan("m_matrix.txt"), nrow=3, ncol=5, byrow=TRUE)
m #diplays the matrix 

'Array
------'
# Define a 3-d array populated with random numbers (sample() function)
b <- array(sample(30), c(3,5,2))
b #displays the 3-d array

'Data Frame
-----------'
# Create two vectors, one with strings and one with integers
name <- c("John", "Jennifer", "Andrew", "Peter", "Christine")
age <- c(30, 45, 23, 56, 32)
# Combine the two vectors in a data frame
data.frame(name, age)


# ----- Useful functions -----
'*****************************'
# Sequence function
seq(1, 3, 0.25) 

# Repeat function
rep(4, 10) 

# Minimum value
min(y) 

# Maximum value
max(y) 

# Measures of central tendency
'-----------------------------'
mean(y)   # Arithmetic mean
median(y) # Median
table(y)  # for finding the mode

# Measures of dispersion
'-----------------------'
mad(y) #mean absolute deviation
var(y) #variance
sd(y)  #standard deviation

# ----- Reading data & Converting into time series -----
'*******************************************************'
# Load quarterly data and convert to time series
ts1 <- ts(scan("ts1.txt"), start=c(2011,1), frequency=4)
# Plot the data, adding a main title and a suitable label for the y-axis
plot(ts1, main="Stationary quarterly data", ylab="Demand")

# Load monthly data and convert to time series
ts2 <- ts(scan("ts2.txt"), start=c(2011,1), frequency=12)
plot(ts2, main="Trend and seasonal monthly data", ylab="Demand")

ts3 <- ts(scan("ts3.txt"), start=c(2011,1), frequency=12)
plot(ts3, main="Intermittent monthly data", ylab="Demand")

