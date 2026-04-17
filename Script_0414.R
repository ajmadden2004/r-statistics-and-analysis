# In-class Exercise 4-14-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass11")

##########################################################################

data("swiss")

?swiss

summary(swiss)


# Multicollinearity

# DSC/BAN 211
cor(swiss) %>% 
  round(2)

library(car)

lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality,
   data = swiss) %>% 
  vif()


lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality,
   data = swiss) %>% 
  summary()


# Best Subsets Regression INCLUDING QUADRATIC TERM AND INTERACTION TERM

# Quadratic Term (squared)
swiss$Agri2 <- swiss$Agriculture^2

# Interaction Term
swiss$Exam_Edu <- swiss$Examination*swiss$Education


library(leaps) # For Best Subsets Regression

regfit <- regsubsets(Fertility ~ ., data = swiss, nvmax = 7) 

summary(regfit) 

bestfit <- summary(regfit)

# Best Fit Models - use this for hw
data.frame( 
  AdjR2 = which.max(bestfit$adjr2),
  BIC = which.min(bestfit$bic)
)

# Ignore high VIF values
lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality +
     Agri2 + Exam_Edu, data = swiss) %>% 
  vif()


##############################################################################
# Dummy Coding (Three Categories)

bikes <- read.csv("bikes.csv")

bikes$Type_Road <- ifelse(bikes$Type == "Road", 1, 0)

bikes$Type_Fitness <- ifelse(bikes$Type == "Fitness", 1, 0)

bikes$Type_Comfort <- ifelse(bikes$Type == "Comfort", 1, 0)

bikes %>% 
  lm(Price ~ Weight + Type,
     data = .) %>% 
  summary()

bikes %>% 
  lm(Price ~ Weight + Type_Road + Type_Fitness + Type_Comfort,
     data = .) %>% 
  summary()

bikes %>% 
  lm(Price ~ Weight + Type_Road + Type_Fitness,
     data = .) %>% 
  summary()

bikes %>% 
  lm(Price ~ Weight + Type_Fitness + Type_Comfort,
     data = .) %>% 
  summary()
# Comfort bikes are $510 cheaper than Road bikes


# Fixed Effects (Year FE)
airfare <- read.csv("airfare.csv")

airfare$y1997 <- ifelse(airfare$year == 1997, 1, 0)

airfare$y1998 <- ifelse(airfare$year == 1998, 1, 0)

airfare$y1999 <- ifelse(airfare$year == 1999, 1, 0)

airfare$y2000 <- ifelse(airfare$year == 2000, 1, 0)


airfare %>% 
  lm(fare ~ dist + y1997 + y1998 + y1999 + y2000,
     data = .) %>% 
  summary()

airfare %>% 
  lm(fare ~ dist + y1998 + y1999 + y2000,
     data = .) %>% 
  summary()







