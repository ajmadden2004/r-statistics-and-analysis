# In-class Exercise 4-21-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass11")

##########################################################################

# Unit 10 - Time Series Regression -- ARIMA Model

# install.packages("forecast")
# 
# install.packages("tseries")

library(forecast)

# library(tseries)

##########################################################################

# Load smartphone sales data
phone <- read.csv("smartphone.csv")

# Data Setting
# Convert Sales into a time series

tsphone <- ts(phone$Sales, frequency = 4, start = c(1,1))

# Time series plot
plot(tsphone)

# Decomposition
decomp <- stl(tsphone, 4)

plot(decomp)

# Run auto.arima - Optimal P, D, Q parameters - ARIMA (P, D, Q) [frequency] - Seasonal ARIMA or ARIMA is seasonal = TRUE

auto.arima(tsphone, seasonal = TRUE)

# Forecasting

arima_model <- auto.arima(tsphone, seasonal = TRUE)

forecast_arima <- forecast(arima_model, h = 8) # 4 quarters * 2 years

plot(forecast_arima)

# Create a data file with forecasted values
forecast_data <- data.frame(
  Period = time(forecast_arima$mean),
  Forecast = as.numeric(forecast_arima$mean)
)

# Forecast Accuracy

accuracy(forecast_arima)

??accuracy

# ME: Mean Error
# 
# RMSE: Root Mean Squared Error
# 
# MAE: Mean Absolute Error
# 
# MPE: Mean Percentage Error
# 
# MAPE: Mean Absolute Percentage Error
# 
# MASE: Mean Absolute Scaled Error
# 
# ACF1: Autocorrelation of errors at lag 1.


###########################################################################

# In-Class Question

covid <- read.csv("covid.csv")

# Data Setting
# Convert Sales into a time series

tscovid <- ts(covid$Cases, frequency = 7, start = c(1,1))


# Time series plot
plot(tscovid)

# Decomposition
decomp <- stl(tscovid, 7)

plot(decomp)

# Run auto.arima - Optimal P, D, Q parameters - ARIMA (P, D, Q) [frequency] - Seasonal ARIMA or ARIMA is seasonal = TRUE

auto.arima(tscovid, seasonal = TRUE)

# Forecasting

arima_model2 <- auto.arima(tscovid, seasonal = TRUE)

forecast_arima2 <- forecast(arima_model2, h = 14) # 7 days * 2 weeks

plot(forecast_arima2)

# Create a data file with forecasted values
forecast_data2 <- data.frame(
  Period = time(forecast_arima2$mean),
  Forecast = as.numeric(forecast_arima2$mean)
)

# Forecast Accuracy

accuracy(forecast_arima2)

??accuracy



























