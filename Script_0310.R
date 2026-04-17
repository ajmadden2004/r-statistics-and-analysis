# In-class Exercise 3-10-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass10")


##########################################################################

# This will help with HW6

data("mtcars")


# Regression
mtcars %>% 
  lm(mpg ~ wt, data =.) %>% 
  summary()

# Residual
mtcars %>% 
  lm(mpg ~ wt, data =.) %>% 
  plot()

# Cook's distance
mtcars$cooks <- mtcars %>% 
  lm(mpg ~ wt, data =.) %>% 
  cooks.distance()

# Scatter without influential case
mtcars %>% 
  filter(cooks < 0.5) %>% 
  ggplot(aes(wt, mpg)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

# Regression without influential case
mtcars %>% 
  filter(cooks < 0.5) %>% 
  lm(mpg ~ wt, data =.) %>% 
  summary()


###################################################################

# Multiple Linear Regression

# Load "trees" data

data(trees)

?trees


# Scatter Plot
trees %>% 
  ggplot(aes(Girth, Volume)) +
  geom_point(aes(col = Height)) +
  geom_smooth(method = lm, se = FALSE)

trees %>% 
  ggplot(aes(Height, Volume)) +
  geom_point(aes(col = Girth)) +
  geom_smooth(method = lm, se = F)


# Regression (analysis summary) (HW6 Q2)
trees %>% 
  lm(Volume ~ Girth + Height, data = .,) %>% 
  summary()
# Interpretation of the data --> 4.71 (slope coefficient for Girth) represents 
# that an increase of 1 inch in the diameter of the tree leads to an increase of
# 4.71 cubic feet in the volume when the height is held constant (ceteris paribus)

# R-squared interpretation (Q3)
# you use adjusted R-squared in multiple linear regression (this means 
# whatever % of the variability in the y variable can be explained by the
# linear relationship between the set of independent variables (list them here)

# Significance of IVs (Q4)
# Most significant IV (strongest predictor): Girth (Q5)
# Slope interpretation (Q7) - Don't forget ceteris paribus
# Equation (Q6): Volume = Intercept + variable 1 + variable 2
#           Volume = -57.9877 + 4.7082*Girth + 0.3393*Height


# Prediction (Q9)
# Girth = 18, Height = 75
trees %>% 
  lm(Volume ~ Girth + Height, data = .,) %>% 
  predict(data.frame(Girth=18, Height=75))


# Residual Analysis (Q10, 11)
trees %>% 
  lm(Volume ~ Girth + Height, data = .,) %>% 
  plot()


# In-class Q3
# Regression
mtcars %>% 
  lm(mpg ~ disp + hp + wt + am, data = .,) %>% 
  summary()

# Prediction # the weight is 2500 lbs but you enter it as 2.5 because of the help sheet says (1000 lbs)
mtcars %>% 
  lm(mpg ~ disp + hp + wt + am, data = .,) %>% 
  predict(data.frame(disp = 200, hp = 100, wt = 2.5, am = 0))





