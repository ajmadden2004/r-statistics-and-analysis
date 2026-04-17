# In-class Exercise 2-17-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass9")


##########################################################################

# if the exam doesn't say confidence level assume 95% with 0.05 significance
# = is assignment
# == is comparison
# only ggplot stuff covered in class on the slides will be on the exam 
# filter rows, select columns

# shift + ctrl + c to comment something out 


library(gapminder)


data("gapminder")


# Density Plot
gapminder %>% 
  filter(continent %in% c("Africa", "Europe", "Americas")) %>%
  ggplot(aes(lifeExp, fill = continent)) +
  geom_density(alpha = 0.4)


# Density Plot (if not filtering continents)
gapminder %>% 
  ggplot(aes(lifeExp, fill = continent)) +
  geom_density(alpha = 0.4)


# ANOVA
gapminder %>% 
  filter(continent %in% c("Africa", "Europe", "Americas")) %>%
  aov(lifeExp ~ continent, data =.) %>% 
  summary()


# Fairwise Comparison - Tukey HSD
gapminder %>% 
  filter(continent %in% c("Africa", "Europe", "Americas")) %>%
  aov(lifeExp ~ continent, data =.) %>% 
  TukeyHSD()


######################################################################

# Goodness of Fit Test

flowers <- read.csv("flowers.csv")

flowers %>% 
  table() %>% 
  prop.table() %>% 
  round(2)

flowers %>% 
  select(Size) %>%
  table() %>% 
  prop.table() %>% 
  round(2)

flowers %>% 
  select(Size) %>%
  table() %>% 
  chisq.test(p = c(0.2, 0.5, 0.3)) # Alphabetical order



# Test of Independence - null is they are independent

flowers %>% 
  table() %>% 
  chisq.test()


flowers %>% 
  select(Size, Species) %>% 
  table() %>% 
  chisq.test()























