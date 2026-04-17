# In-class Exercise 3-24-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass10")


##########################################################################

library(MASS)   # Box Cox

library(rstatix)  # Shapiro

library(gapminder)

data(gapminder)

View(gapminder)


# Regression (Level-Level) - Q1
gapminder %>% 
  filter(continent == "Africa") %>% 
  lm(gdpPercap ~ lifeExp, data =.) %>% 
  summary()


# Log Transformation
gapminder$log_gdpPercap <- log(gapminder$gdpPercap)

gapminder$log_lifeExp <- log(gapminder$lifeExp)


# Regression (Level-Log) - Q2
gapminder %>% 
  filter(continent == "Africa") %>% 
  lm(gdpPercap ~ log_lifeExp, data =.) %>% 
  summary()


# Regression (Log-Level) - Q3
gapminder %>% 
  filter(continent == "Africa") %>% 
  lm(log_gdpPercap ~ lifeExp, data =.) %>% 
  summary()


# Regression (Log-Log) - Q4
gapminder %>% 
  filter(continent == "Africa") %>% 
  lm(log_gdpPercap ~ log_lifeExp, data =.) %>% 
  summary()


# Canvas Q5
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

















