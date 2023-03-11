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

data <- read.csv("~/1. University/Subjects/ECMT6003 - Applied Business Forecasting/Tutorials/Tut 3/AuQuGDP.csv")

head(data)
View(data)
names(data)

'(a). plot GDP & ADF'
plot(data$AuGDP, type ="l")

gdp_ts <- ts(data$AuGDP, start = 1960, end = 2020, frequency = 4)

gdp_ts

acf(data$AuGDP, lag.max = 40)

pacf(data$AuGDP, lag.max = 40)

adf.test(na.omit(data$AuGDP), nlag = 3)

'(b). Differencing data, ploting & ADF'
data$GDP_D1 = data$AuGDP - Lag(data$AuGDP, 1)


GDP_D1_ts = ts(data$GDP_D1, start = 1960,end = 2020, frequency = 4)

GDP_D1_ts

acf(na.omit(data$GDP_D1), lag.max = 40)

pacf(na.omit(data$GDP_D1), lag.max = 40)

adf.test(na.omit(data$GDP_D1), nlag = 3)


'AR(5) Model + predictions'

GDP_2018_ts = window(GDP_D1_ts, start = 1960, end = c(2018,4))

fit_AR <- arima(GDP_2018_ts, order = c(5, 1, 0))

summary(fit_AR)

fit_AR$aic

coeftest(fit_AR)

confint(fit_AR)

checkresiduals(fit_AR)

autoplot(fit_AR)

predict(fit_AR, n.ahead = 1)

predict(fit_AR, n.ahead = 4)

'MA(6) Model + predictions'

fit_MA <- arima(GDP_2018_ts, order = c(0, 1, 6))

summary(fit_MA)

fit_MA$aic

coeftest(fit_MA)

confint(fit_MA)

checkresiduals(fit_MA)

autoplot(fit_MA)

predict(fit_MA, n.ahead = 1)

predict(fit_MA, n.ahead = 4)


