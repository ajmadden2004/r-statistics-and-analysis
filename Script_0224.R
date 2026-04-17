# In-class Exercise 2-24-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass10")


##########################################################################

# do shapiro test to see if you need to do a nonparametic or parametic test

# Unit 6 - Nonparametric

# install.packages("rstatix")
# rstatix helps operate the pipe functions for some non pipe friendly tests


library(rstatix) # Shapiro Test

library(gapminder)

data("gapminder")

library(ggplot2)


# Density Plot
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(lifeExp)) +
  geom_density(fill = "lightblue", alpha = 0.4)


# Q-Q Plot
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(sample = lifeExp)) +
  geom_qq() +
  geom_qq_line()


# Shapiro Test
gapminder %>% 
  filter(continent == "Africa") %>%
  shapiro_test(lifeExp)


# Wilcoxon Signed Rank Test
gapminder %>% 
  filter(continent == "Africa") %>% 
  select(lifeExp) %>% 
  wilcox.test(lifeExp~1, mu = 50, data = .,
              alternative = "two.sided") # alternative is not necessary here








