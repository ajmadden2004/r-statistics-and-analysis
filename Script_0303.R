# In-class Exercise 3-3-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass10")


##########################################################################

# Unit 7 - Regression

# Simple Linear Regression

data(cars)

?cars

# Scatter Plot
cars %>% 
  ggplot(aes(speed, dist)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)


# Regression
cars %>% 
  lm(dist ~ speed, data =.) %>% 
  summary()
 

# Prediction Equation
# dist = -17.5791 + 3.9324 * Speed (x)

# Slope: A one unit increase in X leads to a ?? increase or decrease in Y 
# (rise over run)

# A 1-mile increase in the speed (MPH) leads to an increase of 3.93 feet 
# in the stopping distance


# Prediction
cars %>% 
  lm(dist ~ speed, data =.) %>% 
  predict(data.frame(speed = 20)) %>% 
  round(2)

# cars %>% 
  # lm(dist ~ speed + weight, data =.) %>% 
  # predict(data.frame(speed = 20, weight = 2200)) %>% 
  # round(2)

cars %>% 
  lm(dist ~ speed, data =.) %>% 
  predict(data.frame(speed = c(20, 23, 26))) %>% 
  round(2)

# Extrapolation: Risky in making a prediction
cars %>% 
  lm(dist ~ speed, data =.) %>% 
  predict(data.frame(speed = 40)) %>% 
  round(2)

# Fitted (Predicted) Values to the data
cars$predicted <- cars %>% 
  lm(dist ~ speed, data =.) %>% 
  fitted.values()
  
# Residual Values to the data
cars$residual <- cars %>% 
  lm(dist ~ speed, data =.) %>% 
  residuals()

# Residual Values using Mutate
cars <- cars %>% 
  mutate(resid = dist-predicted)









