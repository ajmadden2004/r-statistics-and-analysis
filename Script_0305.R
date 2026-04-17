# In-class Exercise 3-5-2026

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


#############################################
# Today's class question 03-05-2026

data("mtcars")

# 1. Load the "mtcars" data (built-in data). You want to conduct a regression analysis 
# to find out a relationship between the weight of the vehicle and MPG 
# (miles per gallon; fuel efficiency measure). What is the predicted value of MPG 
# when the weight is 5.5 (i.e., 5500 lbs)? <--- make sure to look the data dictionary
# on the exam because he will give us something like 5500 lbs and we will have to 
# translate it to 5.5 by looking at the data dictionary it says weight (1000lbs) so 
# you do 5500/1000 = 5.5

# Scatter Plot
mtcars %>% 
  ggplot(aes(wt, mpg)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# Regression
mtcars %>% 
  lm(mpg ~ wt, data =.) %>% 
  summary()

# Prediction
mtcars %>% 
  lm(mpg ~ wt, data =.) %>% 
  predict(data.frame(wt = 5.5)) %>% 
  round(2)

# On exams, professor won't say wt = 5.5, rather 5500 lbs
# Be cautious of the units of the independent and dependent variable


# Multiple Regression

# Regression
mtcars %>% 
  lm(mpg ~ wt + hp + disp, data =.) %>% 
  summary()

# Prediction
mtcars %>% 
  lm(mpg ~ wt + hp + disp, data =.) %>% 
  predict(data.frame(wt = 5.5, hp = 100, disp = 300))

##################################################################
# Today's class 03-05-2026

data(cars)

# Residual Analysis

cars %>% 
  lm(dist ~ speed, data =.) %>% 
  plot()

# In-class Question 2
# Residual
mtcars %>% 
  lm(mpg ~ wt, data =.) %>% 
  plot()











