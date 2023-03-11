# clear memory
rm(list = ls())

# load packages
library(AER)
library(readxl)
library(dynlm)
library(vars)
library(quantmod)
library(scales)
library(fGarch)
library(tidyverse)
library(haven)
library(tseries)
library(ggplot2)

?VAR

# Load data 

data <- read_dta("~/1. University/Subjects/ECMT6003 - Applied Business Forecasting/Tutorials/Tut 8/Kilian2009old.dta")

head(data)

View(data)

# set time index

time <- seq(as.Date("1973/2/1"), as.Date("2007/12/1"), "month")
data$time <- time

head(data)

# Graph Series

ggplot(data = data, aes(x=time))+
  geom_line(aes(y=oil), color = "blue")

ggplot(data = data, aes(x = time))+
  geom_line(aes(y = price), color = "red")

ggplot(data = data, aes(x = time))+
  geom_line(aes(y = output), color = "black")

# Transform all data into time series

### Oil

oil <- ts(data$oil, start = c(1973, 2), end = c(2007, 12), frequency = 12)

### Output and its fd

output <- ts(data$output, start = c(1973, 2),  end = c(2007, 12), frequency = 12)
D1_output <- diff(output)

### price and its fd

price <- ts(data$price, start = c(1973, 2),  end = c(2007, 12),  frequency = 12)
D1_price <- diff(price)

price

# Check stationarity of each process

### price
adf.test(price)
adf.test(D1_price)

plot(D1_price)

### Output
adf.test(output)
adf.test(D1_output)

plot(D1_output)

### Oil (no need for fd as process is stationary)
adf.test(oil)

plot(oil)

# Set up the data matrix for VAR process

VAR_data <- window(ts.union(oil, D1_output, D1_price), start = c(1973, 3), end = c(2007, 12))

VARselect(VAR_data, lag.max=8,type="const")[["selection"]]

# VAR(1)

var1 <- VAR(VAR_data, p=1, type = 'const')

summary(var1)

### Coefficient breakdown
coef(var1)

### Eigenvalues
roots(var1, modulus = TRUE)

### Portmanteau-test (Ljung-Box test)
serial.test(var1, lags.pt = 10, type = "PT.asymptotic")

serialtest <- serial.test(var1, lags.pt = 10, type = "PT.asymptotic")

### Visualise test results 
plot(serialtest)

# There are two ways to compute the MA representations

### Firstly, you can compute the coefficient matricies of the MA representation
Phi(var1, nstep = 4)

### Secondly, you can try coefficient matrices of the orthogonalised MA representation
Psi(var1, nstep = 4)

# Lets compute the ARDL for each VAR equation using p=2

VAR_EQ1 <- dynlm(oil ~ L(oil,1:2) + L(D1_output, 1:2) + L(D1_price, 1:2), 
                 start = c(1973, 2), 
                 end = c(2007, 12))

coeftest(VAR_EQ1, vcov. = sandwich)

VAR_EQ2 <- dynlm(D1_output ~ L(D1_output,1:2) + L(oil, 1:2) + L(D1_price, 1:2),
                 start = c(1973, 2), 
                 end = c(2007, 12))

coeftest(VAR_EQ2, vcov. = sandwich)

VAR_EQ3 <- dynlm(D1_price ~ L(D1_price,1:2) + L(oil, 1:2) + L(D1_output, 1:2),
                 start = c(1973, 2), 
                 end = c(2007, 12))

coeftest(VAR_EQ3, vcov. = sandwich)
