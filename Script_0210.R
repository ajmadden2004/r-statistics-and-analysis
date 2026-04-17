# In-class Exercise 2-10-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass7")


##########################################################################


install.packages("gapminder")

library(gapminder)

data()

data("gapminder")

?gapminder


# H0: mu = 50
# Ha: mu =/= 50

# Two tailed test

# Density Plot
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(lifeExp, fill = continent)) +
  geom_density(alpha = 0.4)


gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(lifeExp, fill = continent)) +
  geom_density(alpha = 0.4) +
  geom_vline(aes(xintercept = mean(lifeExp)), color = "red") +
  geom_vline(aes(xintercept = 50), color = "blue")
























