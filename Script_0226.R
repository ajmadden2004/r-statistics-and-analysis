# In-class Exercise 2-26-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass10")


##########################################################################

# do shapiro text to see if you need to do a nonparametic or parametic test

# Unit 6 - Nonparametric

# install.packages("rstatix")
# rstatix helps operate the pipe functions for some non pipe friendly tests


library(rstatix) # Shapiro Test

library(gapminder)

data("gapminder")

library(ggplot2)



# Grouped Summary (e.g., HW5 Q1)
gapminder %>% 
  group_by(continent) %>% 
  summarise(mean(lifeExp))

gapminder %>% 
  filter(continent == "Africa") %>% 
  group_by(continent) %>% 
  summarise(mean(lifeExp))




# Two Group Comparison

# Density Plot
gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>% 
  ggplot(aes(lifeExp, fill = continent)) +
  geom_density(alpha = 0.4)


# Q-Q Plot
gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>% 
  ggplot(aes(sample = lifeExp, color = continent)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~continent)

# Shapiro Test
gapminder %>% 
  filter(continent == "Europe") %>%
  shapiro_test(lifeExp)

# Mann-Whitney U Test (Wilcoxon Rank Sum Test) 
gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>% 
  wilcox.test(lifeExp ~ continent, date =.,
              alternative = "two.sided")



# Levene Test (as an independent test; not a preliminary test for Mann-Whitney)


# Load "car" package (CAR: Companion of Applied Regression)
install.packages("car")

library(car)


gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>% 
  leveneTest(lifeExp ~ continent, data = .,
             alternative = "two.sided")


# Three Group Comparison (Add Americas)
# if one of the samples doesn't follow normal distribution use Kruskal-Wallis instead of ANOVA
gapminder %>% 
  filter(continent %in% c("Africa", "Europe", "Americas")) %>% 
  ggplot(aes(lifeExp, fill = continent)) +
  geom_density(alpha = 0.4)

# Q-Q Plot
gapminder %>% 
  filter(continent %in% c("Africa", "Europe", "Americas")) %>% 
  ggplot(aes(sample = lifeExp, color = continent)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~continent)

# Shapiro Test
gapminder %>% 
  filter(continent == "Americas") %>%
  shapiro_test(lifeExp)

# Kruskal-Wallis Test
gapminder %>% 
  filter(continent %in% c("Africa", "Europe", "Americas")) %>% 
  kruskal.test(lifeExp~continent, data = .)


# Tukey HSD is only needed if you reject the null hypothesis for ANOVA and same for the the nonparametric - only do pairwise rank sum if you reject the null hypothesis for kruskal-wallis

# Pairwise Wilcoxon Rank Sum Test
gapminder %>% 
  filter(continent %in% c("Africa", "Europe", "Americas")) %>% 
  pairwise.wilcox.test(lifeExp~continent, data = .)
# Doesn't work


gapminder2 <- gapminder %>% 
  filter(continent %in% c("Africa", "Europe", "Americas"))

pairwise.wilcox.test(gapminder2$lifeExp, gapminder2$continent)  
# HW5 Question 5



# Correlation Analysis between lifeExp and gdpPercap

# Pearson Correlation (Parametric)

gapminder %>% 
  select(lifeExp, gdpPercap) %>% 
  cor()

gapminder %>% 
  select(lifeExp, gdpPercap) %>% 
  cor(method = "pearson")

#r=.58 (moderate positive correlation)


# Shapiro Test for all continents combined (no filter)

gapminder %>% 
  shapiro_test(lifeExp)

gapminder %>% 
  shapiro_test(gdpPercap)


# Scatter Plot
gapminder %>% 
  select(lifeExp, gdpPercap) %>% 
  ggplot(aes(gdpPercap, lifeExp)) +
  geom_point()


# Scatter Plot
gapminder %>% 
  select(lifeExp, gdpPercap) %>% 
  ggplot(aes(gdpPercap, lifeExp)) +
  geom_point(shape = 20) +
  xlim(0, 40000)

# Add a linear trend line
gapminder %>% 
  select(lifeExp, gdpPercap) %>% 
  ggplot(aes(gdpPercap, lifeExp)) +
  geom_point(shape = 20) +
  xlim(0, 40000) +
  geom_smooth(method = lm, se = FALSE)

# Add a fitted curve (tiny local regression; kernel style)
gapminder %>% 
  select(lifeExp, gdpPercap) %>% 
  ggplot(aes(gdpPercap, lifeExp)) +
  geom_point(shape = 20) +
  xlim(0, 40000) +
  geom_smooth()


# Spearman Correlation (nonparametric)

gapminder %>% 
  select(lifeExp, gdpPercap) %>% 
  cor(method = "spearman")






