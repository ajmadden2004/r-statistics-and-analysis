# In-class Exercise 4-16-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass11")

##########################################################################

# Unit 10 - Time Series Regression

# Load Umbrella data
umbrella <- read.csv("umbrella.csv")
view(umbrella)

# Create Period variable
umbrella$Period <- seq_len(nrow(umbrella))

# seq_len: generates a sequence of length: nrow(umbrella)

#dpylr mutate
umbrella <- umbrella %>% 
  mutate(Sequence = row_number())

# Create Quarter Dummy Variables
umbrella$Qtr1 <- ifelse(umbrella$Quarter == 1, 1, 0)

umbrella$Qtr2 <- ifelse(umbrella$Quarter == 2, 1, 0)

umbrella$Qtr3 <- ifelse(umbrella$Quarter == 3, 1, 0)

umbrella$Qtr4 <- ifelse(umbrella$Quarter == 4, 1, 0)

# Time Series Plot
umbrella %>% 
  ggplot(aes(Period, Sales)) +
  geom_line()

umbrella %>% 
  ggplot(aes(Period, Sales)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE)

# Model 1 - Linear Trend Model (Period Only)
umbrella %>% 
  lm(Sales ~ Period, data =.) %>% 
  summary()

# Model 2 - Seasonality Only
umbrella %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3 + Qtr4, data =.) %>% 
  summary()

umbrella %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3, data =.) %>% 
  summary()

# 6th Year Sales Prediction (Forecasting) - Seasonality Only (w/o trend)
umbrella %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3, data =.) %>% 
  predict(data.frame(Qtr1 = 1, Qtr2 = 0, Qtr3 = 0))

umbrella %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3, data =.) %>% 
  predict(data.frame(Qtr1 = 0, Qtr2 = 1, Qtr3 = 0))

umbrella %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3, data =.) %>% 
  predict(data.frame(Qtr1 = 0, Qtr2 = 0, Qtr3 = 1))

umbrella %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3, data =.) %>% 
  predict(data.frame(Qtr1 = 0, Qtr2 = 0, Qtr3 = 0))

# Model 3 - Seasonality with Trend (Both Qtr dummy variables and Period)
umbrella %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3 + Period, data =.) %>% 
  summary()

# 6th Year Sales Prediction (Forecasting) - Seasonality with trend
umbrella %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3 + Period, data =.) %>% 
  predict(data.frame(Qtr1 = 1, Qtr2 = 0, Qtr3 = 0, Period = 21))
 
umbrella %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3 + Period, data =.) %>% 
  predict(data.frame(Qtr1 = 0, Qtr2 = 1, Qtr3 = 0, Period = 22))

umbrella %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3 + Period, data =.) %>% 
  predict(data.frame(Qtr1 = 0, Qtr2 = 0, Qtr3 = 1, Period = 23))

umbrella %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3 + Period, data =.) %>% 
  predict(data.frame(Qtr1 = 0, Qtr2 = 0, Qtr3 = 0, Period = 24))

# MSE Calculation (Assumption: Training Set = Testing Set)

model <- lm(Sales ~ Qtr1 + Qtr2 + Qtr3 + Period, data = umbrella)

umbrella$forecast <- predict(model)

diff <- umbrella$Sales - umbrella$forecast

diff_sq <- diff^2

mse <- mean(diff_sq)

mse


#############################################################################
# In-Class Practice

# Load Smartphone data
smartphone <- read.csv("smartphone.csv")
view(smartphone)

# Create Period variable
smartphone$Period <- seq_len(nrow(smartphone))

#dpylr mutate
smartphone <- smartphone %>% 
  mutate(Sequence = row_number())

# Create Quarter Dummy Variables
smartphone$Qtr1 <- ifelse(smartphone$Quarter == 1, 1, 0)

smartphone$Qtr2 <- ifelse(smartphone$Quarter == 2, 1, 0)

smartphone$Qtr3 <- ifelse(smartphone$Quarter == 3, 1, 0)

smartphone$Qtr4 <- ifelse(smartphone$Quarter == 4, 1, 0) 

# Time Series Plot
smartphone %>% 
  ggplot(aes(Period, Sales)) +
  geom_line()

smartphone %>% 
  ggplot(aes(Period, Sales)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE)

# Model 1 - Linear Trend Model (Period Only)
smartphone %>% 
  lm(Sales ~ Period, data =.) %>% 
  summary()

# Model 2 - Seasonality Only
smartphone %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3 + Qtr4, data =.) %>% 
  summary()

smartphone %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3, data =.) %>% 
  summary()

# 5th Year Sales Prediction (Forecasting) - Seasonality Only (w/o trend)
smartphone %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3, data =.) %>% 
  predict(data.frame(Qtr1 = 1, Qtr2 = 0, Qtr3 = 0))

smartphone %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3, data =.) %>% 
  predict(data.frame(Qtr1 = 0, Qtr2 = 1, Qtr3 = 0))

smartphone %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3, data =.) %>% 
  predict(data.frame(Qtr1 = 0, Qtr2 = 0, Qtr3 = 1))

smartphone %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3, data =.) %>% 
  predict(data.frame(Qtr1 = 0, Qtr2 = 0, Qtr3 = 0))

# Model 3 - Seasonality with Trend (Both Qtr dummy variables and Period)
smartphone %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3 + Period, data =.) %>% 
  summary()

# 6th Year Sales Prediction (Forecasting) - Seasonality with trend
smartphone %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3 + Period, data =.) %>% 
  predict(data.frame(Qtr1 = 1, Qtr2 = 0, Qtr3 = 0, Period = 17))

smartphone %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3 + Period, data =.) %>% 
  predict(data.frame(Qtr1 = 0, Qtr2 = 1, Qtr3 = 0, Period = 18))

smartphone %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3 + Period, data =.) %>% 
  predict(data.frame(Qtr1 = 0, Qtr2 = 0, Qtr3 = 1, Period = 19))

smartphone %>% 
  lm(Sales ~ Qtr1 + Qtr2 + Qtr3 + Period, data =.) %>% 
  predict(data.frame(Qtr1 = 0, Qtr2 = 0, Qtr3 = 0, Period = 20))

# MSE Calculation (Assumption: Training Set = Testing Set)

model <- lm(Sales ~ Qtr1 + Qtr2 + Qtr3 + Period, data = smartphone)

smartphone$forecast <- predict(model)

diff <- smartphone$Sales - smartphone$forecast

diff_sq <- diff^2

mse <- mean(diff_sq)

mse
















