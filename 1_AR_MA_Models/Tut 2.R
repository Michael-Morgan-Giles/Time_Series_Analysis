library(tidyverse)
library(fpp2)
library(readr)
library(dplyr)
library(astsa)
library(xts)
library(ggplot2)
library(Hmisc)

data <- read.csv("~/1. University/Subjects/ECMT6003 - Applied Business Forecasting/Tutorials/Tut 2/cpi.csv")
data <- na.omit(data)

'plot cpi'
plot(data$CPI, type = "l")

data %>% 
  ggplot(aes(month, CPI))+
  geom_point()+
    geom_line(size = 1)+
  theme_minimal()+
  labs(title = "CPI Plot",
       y = "CPI",
       x = "month")

cpi_ts <- ts(data$CPI, start = 1983, end = 2018, frequency = 12)

cpi_ts

"acf plot for CPI"
acf(data$CPI, lag.max = 40)

"create infl variable"

data$infl <- (data$CPI - lag(data$CPI, 12)) / lag(data$CPI, 12)

infl_ts <- ts(data$infl, start = 1983, end = 2018, frequency = 12)

infl_ts

"plot infl"
plot(data$infl, type = "l")

acf(na.omit(data$infl), lag.max = 40)
