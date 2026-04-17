# In-class Exercise 4-9-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass11")

##########################################################################

data("swiss")

?swiss

summary(swiss)

lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality,
   data = swiss) %>% 
  summary()


# DONT need to include any of the regressions on cheat sheet for final exam

##########################################################################
# Stepwise Regression

# Preliminary Analysis - Simple Regressios - Order IVs

lm(Fertility ~ Agriculture, data = swiss) %>% 
  summary()
# 0.0149

lm(Fertility ~ Examination, data = swiss) %>% 
  summary()
# 9.45e-07

lm(Fertility ~ Education, data = swiss) %>% 
  summary()
# 3.66e-07

lm(Fertility ~ Catholic, data = swiss) %>% 
  summary()
# 0.00103

lm(Fertility ~ Infant.Mortality, data = swiss) %>% 
  summary()
# 0.00359

# Order: Education, Examination, Catholic, Infant.Mortality, Agriculture

# Model 1 (Education Only)
lm(Fertility ~ Education, data = swiss) %>% 
  summary()

# Model 2 (Added Examination)
lm(Fertility ~ Education + Examination, data = swiss) %>% 
  summary()

# Model 3 (Added Catholic)
lm(Fertility ~ Education + Examination + Catholic, data = swiss) %>% 
  summary()

# Model 4 (Removed Examination)
lm(Fertility ~ Education + Catholic, data = swiss) %>% 
  summary()

# Model 5 (Added Infant.Mortality)
lm(Fertility ~ Education + Catholic + Infant.Mortality, data = swiss) %>% 
  summary()

# Model 6 (Added Agriculture) - BEST FIT MODEL
lm(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture, data = swiss) %>% 
  summary()

lm(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture, data = swiss) %>% 
  BIC() # Smaller BIC value

# Model 7 (Added Examination)
lm(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture + Examination, data = swiss) %>% 
  summary()

lm(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture + Examination, data = swiss) %>% 
  BIC()

##########################################################################
# Forward Selection - can't go back and get rid of a variable once you hit enter

# Order: Education, Examination, Catholic, Infant.Mortality, Agriculture

# Model 1 (Education Only)
lm(Fertility ~ Education, data = swiss) %>% 
  summary()

# Model 2 (Added Examination)
lm(Fertility ~ Education + Examination, data = swiss) %>% 
  summary()

# Model 3 (Added Catholic)
lm(Fertility ~ Education + Examination + Catholic, data = swiss) %>% 
  summary()

# Model 4 (Added Infant.Mortality)
lm(Fertility ~ Education + Examination + Catholic + Infant.Mortality, data = swiss) %>% 
  summary()

# Model 5 (Added Agriculture) - BEST FIT MODEL
lm(Fertility ~ Education + Examination + Catholic + Infant.Mortality + Agriculture, data = swiss) %>% 
  summary()

##########################################################################
# Backward Selection

# Model 1 (All IVs)
lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality,
   data = swiss) %>% 
  summary()

# Model 2 (Remove Examination - bc it has the largest p-value) - BEST FIT MODEL
lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality,
   data = swiss) %>% 
  summary()

##########################################################################
# Best Subsets Regression

# install.packages("leaps")

library(leaps) # For Best Subsets Regression

regfit <- regsubsets(Fertility ~ ., data = swiss, nvmax = 5) # nvmax is the max number of IVs

summary(regfit) # if you want to use 1 IV the first line selects the best IV with the star, 2 selects the best 2 IVs with stars and so on

bestfit <- summary(regfit)

# Best Fit Models - use this for hw
data.frame( 
  AdjR2 = which.max(bestfit$adjr2),
  BIC = which.min(bestfit$bic)
)

# AdjR2 BIC
#     5   4

# Model that minimizes BIC
lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality,
   data = swiss) %>% 
  summary()

lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality,
   data = swiss) %>% 
  BIC()

##########################################################################
# Multicollinearity

# DSC/BAN 211
cor(swiss) %>% 
  round(2)

library(car)

lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality,
   data = swiss) %>% 
  vif()



