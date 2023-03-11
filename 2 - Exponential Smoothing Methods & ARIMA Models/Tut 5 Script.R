rm(list = ls())

library(tidyverse)
library(fpp2)
library(readr)
library(dplyr)
library(astsa)
library(xts)
library(ggplot2)

data <- read.csv("~/1. University/Subjects/ECMT6003 - Applied Business Forecasting/Tutorials/Tut 5/ausunemp.csv")

View(data)

colnames(data)

names(data)[names(data) == "Unemployment rate (percentage; source: ABS)"] <- "Y"
data = as.matrix(na.omit(data))
data = na.omit(data)
data = data[ , c("Y")]

data = ts(as.numeric(data), start=c(1960, 1), frequency=12)
head(data)

summary(data)

autoplot(data) +
  ylab("unemp") + xlab("Year") + coord_fixed()

'Simple Exponential Smoothing'

ses.data <- ses(data, h = 60)

round(accuracy(ses.data),2)

autoplot(ses.data) +
  autolayer(fitted(ses.data), series="Fitted") +
  ylab("Y") + xlab("Year") + coord_fixed()

'Holts Linear Trend Method'

holt.data <-  holt(data, h=60)

holt.data$model

autoplot(holt.data) +
  autolayer(fitted(holt.data), series = 'Fitted')+
  ylab('unemp') + xlab('Year') +coord_fixed()

autoplot(decompose(data))

'additive smoothing'

data.hw <- ets(data, model = "AAA")

autoplot(forecast(data.hw, h=60))+
  autolayer(fitted(data.hw), series = "Fitted")+
  ylab('unemp')+xlab('year')+coord_fixed()

summary(data.hw)

checkresiduals(data.hw)


'multiplicative smoothing'

data.hw2 <- ets(data, model = "MAM")

autoplot(forecast(data.hw2, h=60))+
  autolayer(fitted(data.hw2), series = "Fitted")+
  ylab('unemp')+xlab('year')+coord_fixed()

summary(data.hw2)

checkresiduals(data.hw2)

'Hotl Winters Seasonal Smoothing'

fit1 <- hw(data,seasonal="additive")
fit2 <- hw(data,seasonal="multiplicative")
autoplot(data) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("unemp") +
  ggtitle("unemployment rate") +
  guides(colour=guide_legend(title="Forecast")) + 
  coord_fixed()

'Holt Winters Seasonal Smoothing With Dampening (Gardner-McKensie)'

fit3 <- hw(data,damped=TRUE,seasonal="additive")
fit4 <- hw(data,damped=TRUE,seasonal="multiplicative")
autoplot(data) +
  autolayer(fit3, series="HW damped additive forecasts", PI=TRUE) +
  autolayer(fit4, series="HW damped multiplicative forecasts",
            PI=TRUE) +
  xlab("Year") +
  ylab("unemp") +
  ggtitle("unemployment rate") +
  guides(colour=guide_legend(title="Forecast")) + 
  coord_fixed()
