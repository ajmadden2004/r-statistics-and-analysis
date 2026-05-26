# In-class Exercise 4-28-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass11")

##########################################################################

# Load data
admission2 <- read.csv("admission2.csv")

# Descriptive Statistics
prop.table(table(admission2$admitted))

summary(admission2$gre)

summary(admission2$gpa)

prop.table(table(admission2$rank))

summary(admission2$rank)
# Ordinal Discrete data

# Logistic Regression
glm(admitted ~ gpa + gre + rank, data = admission2,
    family = "binomial") %>% 
  summary()

# Strongest Predictor: rank
# Weakest Predictor: gre

model2 <- glm(admitted ~ gpa + gre + rank, data = admission2,
              family = "binomial")

# Marginal Effects
marginal2 <- mean(dlogis(predict(model2)))

marginal2*coef(model2)

# rise over run style

# In general, a 1 point increase in gpa leads to 15.2% increase in admission to
# grad school, when all other IVs are held constant (ceteris paribus)

# In general, a 1 point increase in gre leads to 0.045% increase in admission to
# grad school, when all other IVs are held constant (ceteris paribus)

# In general, a 1 point increase in rank leads to -10.9% decrease in admission to
# grad school, when all other IVs are held constant (ceteris paribus)

# Add predicted admission rate to the data
admission2$predicted <- predict(model2, type = "response")

# Prediction of a case
glm(admitted ~ gpa + gre + rank, data = admission2,
    family = "binomial") %>% 
  predict(data.frame(gpa = 3.0, gre = 650, rank = 2)) %>% 
  plogis()

# Predicted Admission Decision
admission2$pred_adm <- ifelse(admission2$predicted > 0.5, 1, 0)

admission2$match <- admission2$admitted - admission2$pred_adm

# Create Confusion Matrix
# install.packages("caret")

library(caret)

conf_matrix <- confusionMatrix(factor(admission2$pred_adm),
                               factor(admission2$admitted))

conf_matrix$overall['Accuracy']
conf_matrix$byClass['Sensitivity']
conf_matrix$byClass['Specificity']
conf_matrix$byClass['Precision']
conf_matrix$byClass['F1']


###########################################################################
# Admission1 data
admission1 <- read.csv("admission1.csv")

model <- glm(admitted ~ gpa, data = admission1,
             family = "binomial")

# Marginal Effects (for better interpretability of slope coeff)
marginal <- mean(dlogis(predict(model)))

marginal*coef(model) # 1 point increase in gpa (the variable listed in the answer) equals 46% increase in adm_rate

# Add predicted admission rate to the data
admission1$predicted <- predict(model, type = "response")

# Predicted Admission Decision
admission1$pred_adm <- ifelse(admission1$predicted > 0.5, 1, 0)

admission1$match <- admission1$admitted - admission1$pred_adm

library(caret)

conf_matrix <- confusionMatrix(factor(admission1$pred_adm),
                               factor(admission1$admitted))

conf_matrix$overall['Accuracy']
conf_matrix$byClass['Sensitivity']
conf_matrix$byClass['Specificity']
conf_matrix$byClass['Precision']
conf_matrix$byClass['F1']





