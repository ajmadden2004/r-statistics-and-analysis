# In-class Exercise 4-23-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass11")

##########################################################################

# Admission 1 Data (College Level)
admission1 <- read.csv("admission1.csv")

summary(admission1$admitted)

table(admission1$admitted)
prop.table(table(admission1$admitted))

summary(admission1$gpa)

# Scatter Plot
ggplot(admission1, aes(gpa, admitted)) +
  geom_point()

ggplot(admission1, aes(gpa, admitted)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

# Jitter Plot
ggplot(admission1, aes(gpa, admitted)) +
  geom_jitter()

ggplot(admission1, aes(gpa, admitted)) +
  geom_jitter(alpha = 0.1)

ggplot(admission1, aes(gpa, admitted)) +
  geom_jitter(height = 0.02, alpha = 0.1) 

ggplot(admission1, aes(gpa, admitted)) +
  geom_jitter(height = 0.02, alpha = 0.1) +
  geom_smooth(method = "lm", se = F)

ggplot(admission1, aes(gpa, admitted)) +
  geom_jitter(height = 0.02, alpha = 0.1) +
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))

# Generate Admission Rate (Probability)
adm_prob <- admission1 %>% 
  group_by(gpa) %>% 
  summarize(adm_rate = mean(admitted),
            count = n())

adm_prob %>% 
  print(n=21)

# Plot Admission Rate
ggplot(adm_prob, aes(gpa, adm_rate)) +
  geom_point()

ggplot(adm_prob, aes(gpa, adm_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

ggplot(adm_prob, aes(gpa, adm_rate)) +
  geom_point() +
  geom_smooth(method = "glm", se = F,
              method.args = list(family = "binomial"))
 
# None of the stuff above will appear on the exam or hw 10


# Logistic Regression
glm(admitted ~ gpa, data = admission1,
    family = "binomial") %>% 
  summary()

model <- glm(admitted ~ gpa, data = admission1,
    family = "binomial")

# Marginal Effects (for better interpretability of slope coeff)
marginal <- mean(dlogis(predict(model)))

marginal*coef(model) # 1 point increase in gpa (the variable listed in the answer) equals 46% increase in adm_rate

# Add predicted admission rate to the data
admission1$predicted <- predict(model, type = "response")

# Predict admission rate using a specific GPA (GPA = 3.22)
glm(admitted ~ gpa, data = admission1,
    family = "binomial") %>% 
  predict(data.frame(gpa = 3.22))

plogis(1.103175)

# One step vs two steps above
glm(admitted ~ gpa, data = admission1,
    family = "binomial") %>% 
  predict(data.frame(gpa = 3.22)) %>% 
  plogis()

# Additional Marginal Effect Calculation Practices
gpa2.0 <- glm(admitted ~ gpa, data = admission1,
              family = "binomial") %>% 
  predict(data.frame(gpa = 2.0)) %>% 
  plogis()

gpa2.1 <- glm(admitted ~ gpa, data = admission1,
              family = "binomial") %>% 
  predict(data.frame(gpa = 2.1)) %>% 
  plogis()


gpa2.1 - gpa2.0


gpa3.0 <- glm(admitted ~ gpa, data = admission1,
              family = "binomial") %>% 
  predict(data.frame(gpa = 3.0)) %>% 
  plogis()

gpa3.1 <- glm(admitted ~ gpa, data = admission1,
              family = "binomial") %>% 
  predict(data.frame(gpa = 3.1)) %>% 
  plogis()


gpa3.1 - gpa3.0

# For HW, if you need to add another variable like sat it will look like this:
glm(admitted ~ gpa + sat, data = admission1,
    family = "binomial") %>% 
  predict(data.frame(gpa = 3.22, sat =)) %>% 
  plogis()


###################################################################
# Practice

admission2 <- read.csv("admission2.csv")

glm(admitted ~ gpa + gre + rank, data = admission2,
    family = "binomial") %>% 
  summary()

model2 <- glm(admitted ~ gpa + gre + rank, data = admission2,
             family = "binomial")

# Marginal Effects (for better interpretability of slope coeff)
marginal <- mean(dlogis(predict(model2)))

marginal*coef(model2) 

# Add predicted admission rate to the data
admission2$predicted <- predict(model2, type = "response")

# Predict admission rate using a specific GPA, GRE, RANK (GPA = 3.00, GRE = 650, Rank = 2)
glm(admitted ~ gpa + gre + rank, data = admission2,
    family = "binomial") %>% 
  predict(data.frame(gpa = 3.00, gre = 650, rank = 2)) %>%  
  plogis()

# 32%

