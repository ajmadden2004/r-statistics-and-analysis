# In-class Exercise 3-26-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass10")


##########################################################################

library(MASS) # Box cox

detach(package:MASS)

?mtcars

data("mtcars")

# Scatter Plot with Regression Line
mtcars %>% 
  ggplot(aes(disp, mpg)) +
  geom_point() +
  geom_smooth(method = lm, se = F)


# Regression (IV = disp, DV = mpg)
mtcars %>% 
  lm(mpg ~ disp, data =.) %>% 
  summary()


# Residual Analysis
mtcars %>% 
  lm(mpg ~ disp, data =.) %>% 
  plot()


# Box Cox to identify optimal lambda
bc <- mtcars %>% 
  boxcox(mpg ~ disp, data =.)


optimal_lambda <- bc$x[which.max(bc$y)]

optimal_lambda



# Canvas Q6-9


# Regression (Level-Level) - Q1
mtcars %>%
  lm(mpg ~ disp, data =.) %>% 
  summary()


# Log Transformation
mtcars$log_mpg <- log(mtcars$mpg)

mtcars$log_disp <- log(mtcars$disp)


# Regression (Level-Log) - Q2
mtcars %>%
  lm(mpg ~ log_disp, data =.) %>% 
  summary()


# Regression (Log-Level) - Q3
mtcars %>%
  lm(log_mpg ~ disp, data =.) %>% 
  summary()


# Regression (Log-Log) - Q4
mtcars %>%
  lm(log_mpg ~ log_disp, data =.) %>% 
  summary()


#####################################################################
# Prediction (disp = 150)

# Regression (Level-Level)
mtcars %>%
  lm(mpg ~ disp, data =.) %>% 
  predict(data.frame(disp = 150))


# Regression (Level-Log)
mtcars %>%
  lm(mpg ~ log_disp, data =.) %>% 
  predict(data.frame(log_disp = log(150)))

log(150)


# Regression (Log-Level)
mtcars %>%
  lm(log_mpg ~ disp, data =.) %>% 
  predict(data.frame(disp = 150)) %>% 
  exp()

exp(3.128261)


# Regression (Log-Log)
mtcars %>%
  lm(log_mpg ~ log_disp, data =.) %>% 
  predict(data.frame(log_disp = log(150))) %>% 
  exp()



########################################################################
# Multiple Linear Regression (make sure on the exam when asked for the interpretation of the slope include (when the whatever variables are held constant (ceteris paribus))

# Regression (Log-Log)
mtcars %>%
  lm(log_mpg ~ log_disp + wt, data =.) %>% 
  summary()


















