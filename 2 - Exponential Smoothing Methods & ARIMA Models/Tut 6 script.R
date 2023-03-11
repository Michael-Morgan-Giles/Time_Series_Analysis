# clear memory
rm(list = ls())

# load packages
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
library(FitAR)

'Q2.'
# load data
data <- read.csv("~/1. University/Subjects/ECMT6003 - Applied Business Forecasting/Tutorials/Tut 6/AuQuGDP.csv")
data <- na.omit(data)

# ACF, PCF, ADF
plot_tsf <- function(data) {
  
  adf.test(data, nlag=3)
  
  par(mfrow=c(2,2))
  
  plot(data, type="l", main="line plot")
  
  acf(data, lag.max = 20, main="ACF")
  
  pacf(data, lag.max = 20, main="PACF")
  
  hist(data, main="histogram")
  
}

plot_tsf(data$AuGDP)

# 1st Difference Check
data$AuGDP_D1 = data$AuGDP - Lag(data$AuGDP, 1)
plot_tsf(data$AuGDP_D1[2:length(data$AuGDP_D1)])

# 2nd Difference Check
data$AuGDP_D2 = data$AuGDP_D1 - Lag(data$AuGDP_D1, 1)
plot_tsf(data$AuGDP_D2[3:length(data$AuGDP_D1)])


# optimal ARIMA model searching
# first, let's set up the ARIMA based on the ACF and PACF
# the model you choose as such will inevitably be hilarious; 
# however, let's start from the funny one

# firstly, set up the time series
GDP = ts(data$AuGDP, start = 1960,end = 2020, frequency = 4)

GDP_2018 = window(GDP, start = 1960, end = c(2018,4))

GDP_2018

# lets see ARIMA(7,2,7) based on the graph
fit <- Arima(GDP_2018, order=c(7,2,7))
summary(fit)

fit$bic
fit$aic
fit$aicc

# making choice based on ACF and PACF is naive. 
# You got many NaN in the return, suggesting that your model has problem

coeftest(fit)

confint(fit)

checkresiduals(fit)

# the roots is too close to the unit circle, which confirms the issue

autoplot(fit)

predict(fit,n.ahead = 4)

# now let's use the function "auto.arima" to choose the value 
# of p,q and d automatically based on BIC and AICc

fit_opt <- auto.arima(GDP_2018)
summary(fit_opt)

# the fit is much better

fit_opt$bic
fit_opt$aic
fit_opt$aicc

# no NaN any more in test and confidence interval

coeftest(fit_opt)

confint(fit_opt)

# Ljung and Box are both happy

checkresiduals(fit_opt)

# roots are well within the unit circle

autoplot(fit_opt)

predict(fit_opt,n.ahead = 4)

'Q3.'

# load in data
data <- read.csv("~/1. University/Subjects/ECMT6003 - Applied Business Forecasting/Tutorials/Tut 6/ausunemp.csv")
data <- data[,0:2]
data <- na.omit(data)

data = subset(data, select = -c(month) )

tail(data)

# add 60 new rows at the end of the table

for (i in 1:60){
  data[nrow(data) + 1,] = NA
} 

tail(data)

# check summary stats and add variables 

describe(data)

data$t = seq(1,length(data$unemp))
data$t2 = data$t^2
data$t3 = data$t^3
data$t4 = data$t^4

mth = rep(seq(1,12), times = ceiling(length(data$t)/12))
data$Month = mth[1:length(data$t)]

head(data,12)
tail(data,12)

# line plot

theme_set(theme_minimal())

p1 = ggplot(data, aes(x=t)) + 
  geom_line(aes(y = unemp, color="unemp")) +
  scale_colour_manual("", values = c("unemp"="black"))

p1

# Now let's remove noise. Firstly, let's compute MA to average out the noise

data$ma = (lag(data$unemp,6) + lag(data$unemp,5) + lag(data$unemp,4) + lag(data$unemp,3) + lag(data$unemp,2) + lag(data$unemp,2) + data$unemp + lead(data$unemp,1) + lead(data$unemp,2) + lead(data$unemp,3) + lead(data$unemp,4) + lead(data$unemp,5)) / 12

p2 = ggplot(data, aes(x=t)) + 
  geom_line(aes(y = unemp, color="unemp")) + 
  geom_line(aes(y = ma, color="ma")) +
  scale_colour_manual("", values = c("unemp"="black", "ma"="red"))

p2

# Compute CMA

data$cma = (data$ma + lead(data$ma,1)) / 2

p3 = ggplot(data, aes(x=t)) + 
  geom_line(aes(y = unemp, color="unemp")) + 
  geom_line(aes(y = cma, color="cma")) +
  scale_colour_manual("", values = c("unemp"="black", "cma"="red"))

p3

# Compute Y/CMA 

data$y_over_cma = data$unemp / data$cma

p4 = ggplot(data, aes(x=t)) + 
  geom_line(aes(y = y_over_cma, color="y_over_cma")) +
  scale_colour_manual("", values = c("y_over_cma"="black"))

p4

# after removing noise from unemp (which returns y/CMA), let's extract 
# the seasonality for Y/CMA

OLS_y_over_cma = lm(y_over_cma ~ as.factor(Month) - 1, data=data)

summary(OLS_y_over_cma)

data$snbar = predict(OLS_y_over_cma, data)

p5 = ggplot() + 
  geom_line(data=data, aes(x = t, y = snbar, color="snbar")) +
  scale_colour_manual("", values = c("snbar"="black"))

p5

# careful: the regression trick that we used during the lecture to find 
# the mean of the snbar doesn't work here, because there are more 
# Januaries-Julies than Augusts-Decembers in this data set. 
# We can fix that by only regressing over the first year 
# (or any year, really... or any period that includes equally many 
# observations for each month)

snbarconst = mean(data$snbar[1:12])

data$sn = data$snbar / snbarconst

p6 = ggplot(data, aes(x=t)) + 
  geom_line(aes(y = sn, color="sn")) +
  scale_colour_manual("", values = c("sn"="black"))

p6

# now lets remove the seasonality from unemployment and generate the trend

data$d = data$unemp / data$sn

p7 = ggplot(data, aes(x=t)) + 
  geom_line(aes(y = d, color="d")) + 
  geom_line(aes(y = unemp, color="unemp")) +
  scale_colour_manual("", values = c("unemp"="black", "d"="red"))

p7

# now let's use polynomial of time to extract the trend component

OLS_d = lm(d ~ t + t2 + t3 + t4, data=data)

summary(OLS_d)

pred_d = predict(OLS_d, data, se.fit=TRUE, interval="confidence")

data$tr = pred_d$fit[,1]

p8 = ggplot() + 
  geom_line(data=data, aes(x = t, y = tr, color="tr")) + 
  geom_line(data=data, aes(x = t, y = unemp, color="black")) +
  scale_colour_manual("", values = c("unemp"="black", "tr"="red"))

p8

# looks decent. not perfectly fitting the early 1990s unemployment peak, 
# but maybe we can attribute the difference to a cyclical swing. 
# we could add yet another power to that polynomial... but four is already
# feeling uncomfortably large

data$trsn = data$tr * data$sn

p9 = ggplot() + 
  geom_line(data=data, aes(x = t, y = trsn, color="trsn")) + 
  geom_line(data=data, aes(x = t, y = unemp, color="unemp")) +
  scale_colour_manual("", values = c("unemp"="black", "trsn"="red"))

p9

# get the standard errors for these forecasts and construct the prediction 
# interval of the seasonality and trend

data$se_trsn = pred_d$se.fit
data$trsn_lo = data$trsn - 1.96 * data$se_trsn
data$trsn_hi = data$trsn + 1.96 * data$se_trsn

# now let's compute the cylce and irregular component interaction and isolate
# the cycle and IR from each other by using moving average again. 
# Finally, let's compute the interaction of the trend, cycle and seasonality

data$clir   = data$d / data$tr
data$cl     = (lag(data$clir,1) + data$clir + lead(data$clir,1))/3

data$cl[1]   = (2*data$clir[1]+data$clir[2])/3
data$cl[355] = (2*data$clir[355]+data$clir[354])/3

loc_nna = which(!is.na(data$unemp))
loc_na  = which(is.na(data$unemp))

data$cl[loc] = 1

data$ir     = data$clir / data$cl
data$trsncl = data$trsn * data$cl

# Now lets plot it all together

p10 = ggplot() +
  geom_line(data=data[loc_nna,], aes(x = t, y = unemp, color="unemp")) +
  geom_line(data=data[loc_nna,], aes(x = t, y = tr, color="tr")) +
  geom_line(data=data[loc_nna,], aes(x = t, y = trsn, color="trsn")) +
  geom_line(data=data[loc_nna,], aes(x = t, y = trsncl, color="trsncl")) +
  scale_colour_manual("", values = c("unemp"="black", "tr"="red", "trsn"="blue", "trsncl"="orange"))

p10

p11 = ggplot() + 
  geom_line(data=data[loc_nna,], aes(x = t, y = sn, color="sn")) + 
  geom_line(data=data[loc_nna,], aes(x = t, y = cl, color="cl")) + 
  geom_line(data=data[loc_nna,], aes(x = t, y = ir, color="ir")) +
  scale_colour_manual("", values = c("sn"="black", "cl"="red", "ir"="blue"))

p11

# this all seems relatively reasonable. Lets plot the prediction interval

p12 = ggplot() + 
  geom_line(data=data, aes(x = t, y = unemp, color="unemp")) + 
  geom_line(data=data, aes(x = t, y = trsn, color="trsn")) + 
  geom_line(data=data, aes(x = t, y = trsn_hi, color="trsn_hi")) + 
  geom_line(data=data, aes(x = t, y = trsn_lo, color="trsn_lo")) +
  scale_colour_manual("", values = c("unemp"="black", "trsn"="red", "trsn_hi"="blue", "trsn_lo"="orange"))

p12
