### Clear Memory
rm(list = ls())

### Load Packages
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
library(foreign)

'Q2'
### load data

data <- read_dta("~/1. University/Subjects/ECMT6003 - Applied Business Forecasting/Tutorials/Tut 9/Kilian2009old.dta")

View(data)

# set up a nice time label
time <- seq(as.Date("1973/2/1"), as.Date("2007/12/1"), "month")

data$time <- time


# Transform all data into time series

### Oil

oil <- ts(data$oil, start = c(1973, 2), end = c(2007, 12), frequency = 12)

### Output and its fd

output <- ts(data$output, start = c(1973, 2),  end = c(2007, 12), frequency = 12)
D1_output <- diff(output)

### price and its fd

price <- ts(data$price, start = c(1973, 2),  end = c(2007, 12),  frequency = 12)
D1_price <- diff(price)

### we have verified that we need to first difference output and price in last week's video.
### Hence, we can skip the ADF/KPSS/... tests. let's start the VAR modelling by selecting the order

VAR_data <- window(ts.union(oil, D1_output, D1_price), start = c(1973,3), end = c(2007,12))

VARselect(VAR_data, lag.max = 8,type = 'const')[["selection"]]

### Lets use BIC to train the model

var1 <- VAR(VAR_data, p=1, type = 'const')

summary(var1)

coef(var1)

### lets plot all the IRF graphs

irf_var1 <- irf(var1)
plot(irf_var1)

### we can choose to only plot the IRF functions from a specific variable

irf_var1_oil <- irf(var1, impulse = "oil", response = c("oil", "D1_price", "D1_output"))
plot(irf_var1_oil)

### we can also choose to only plot the IRF function from variable A to variable B

irf_var1_oil_price <- irf(var1, impulse = "oil", response = "D1_price")
plot(irf_var1_oil_price)

### now let's change the row order in Y and reselect the order, retrain your model, and replot the result

VAR2_data <- window(ts.union(D1_output, oil, D1_price), start = c(1973, 3), end = c(2007, 12))

VARselect(VAR2_data, lag.max=8,type="const")[["selection"]]

var2 <- VAR(VAR2_data, p=1, type="const")

summary(var2)

coef(var2)

irf_var2 <- irf(var2)
plot(irf_var2)

irf_var2_oil_price <- irf(var2, impulse = "oil", response = "D1_price")
plot(irf_var2_oil_price)

'Q3'
data <- read_dta("~/1. University/Subjects/ECMT6003 - Applied Business Forecasting/Tutorials/Tut 9/SWecmt.dta")

head(data)

View(data)

### transform your data into time series and set up a nice time label

hwg <- ts(data$hwg, start = c(1959, 1), end = c(2017, 3), frequency = 4)

gdpg <- ts(data$gdpg, start = c(1959, 1),  end = c(2017, 3), frequency = 4)

difinf <- ts(data$difinf, start = c(1959, 1),  end = c(2017, 3),  frequency = 4)

difinf

gdpg

hwg

### Set up VAR Data

VAR_data <- window(ts.union(difinf, gdpg, hwg), start = c(1959, 3), end = c(2017, 3))

### let's start the VAR modelling by selecting the order

VARselect(VAR_data, lag.max=8,type="const")[["selection"]]

var1 <- VAR(VAR_data, p=1, type="const")

summary(var1)

coef(var1)

roots(var1, modulus = TRUE)

### Now lets impose some long run restrictions

restrict <- matrix(c(1, 1, 1, 1,
                     0, 1, 1, 1, 
                     0, 0, 1, 1),
                   nrow=3, ncol=4, byrow=TRUE)

var2 <- restrict(var1, method = "man", resmat = restrict)

summary(var2)

coef(var2)

### Now lets plot the IRF

irf_var2 <- irf(var2)
plot(irf_var2)

### Now lets decompose the forecast error variance

dec = fevd(var2, n.ahead = 5)

dec

plot(dec)

### some predictions

predict(var2, n.ahead = 8, ci = 0.95)

plot(predict(var2, n.ahead = 8, ci = 0.95))
