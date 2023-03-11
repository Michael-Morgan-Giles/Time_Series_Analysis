# clear memory
rm(list = ls())


### load packages

library(foreign)
library(fda)
library(AER)
library(sandwich)
library(geigen)
library(tseries)
library(urca)
library(tidyverse)
library(fpp2)
library(readr)
library(dplyr)
library(astsa)
library(xts)
library(ggplot2)
library(Hmisc)
library(aTSA)
library(lmtest)
library(fGarch)
library(forecast)

# load Data
data <- read.csv("~/1. University/Subjects/ECMT6003 - Applied Business Forecasting/Tutorials/Tut 12/INTC.csv")

head(data)

# extract Close as a time series

Close <- ts(data$Close, start = c(2000, 11), end = c(2022, 10), frequency = 12)

head(Close, 14)

# generate L1_close and return
L1_Close <- na.remove(Lag(Close, 1))

Return <- na.remove(Close/L1_Close - 1)

head(Return, 13)

# drop first observation of Close
Close <- window(Close, start = c(2000, 12), end = c(2022, 10))

head(Return,13)

# line plot of Return
plot(Return)

# ACF of Return
acf(Return, lag.max = 10)

# ADF of Return
adf.test(Return, nlag = 10)

# Ljung-Box Test
Box.test(Return, lag = 10, type = "Ljung")

# use auto.arima to determine arima model
AR <- auto.arima(Return)

summary(AR)

coeftest(AR)

AR <- Arima(Return, order = c(2,0,1))

summary(AR)

coeftest(AR)

confint(AR)

# generate Return^2
Return2 <- Return^2

head(Return2, 13)

# generate the lag 1 for Return^2
L1_Return2 <- na.remove(Lag(Return2, 1))

head(L1_Return2, 12)

Return2 <- window(Return2, start = c(2001, 2), end = c(2022, 10))

head(Return2, 12)

# AR(1) on Return^2
AR1_return2 <- lm(Return2 ~ L1_Return2)

summary(AR1_return2)

# ARCH(1)
arch.fit <- garchFit(~garch(1,0), data = Return, trace = F)
summary(arch.fit)

#GARCH(1,1)
garch.fit <- garchFit(~garch(1,1), data = Return, trace = F)
summary(garch.fit)
