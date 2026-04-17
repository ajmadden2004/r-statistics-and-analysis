# In-class Exercise 2-12-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass8")


##########################################################################

# shift + ctrl + c to comment something out 


library(gapminder)


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




gapminder %>% 
  filter(continent == "Africa") %>% 
  select(lifeExp) %>% 
  t.test(mu = 50)

gapminder %>% 
  filter(continent == "Africa") %>% 
  select(lifeExp) %>% 
  t.test(mu = 50,
         alternative = "two.sided")

gapminder %>% 
  filter(continent == "Africa") %>% 
  select(lifeExp) %>% 
  t.test(mu = 50,
         alternative = "less")

gapminder %>% 
  filter(continent == "Africa") %>% 
  select(lifeExp) %>% 
  t.test(mu = 50,
         alternative = "less",
         conf.level = 0.99)

gapminder %>% 
  filter(continent == "Africa") %>% 
  select(lifeExp) %>% 
  t.test(mu = 50,
         alternative = "greater")
# Practically, conducting this test is pointless.


###########################################################################
# Two sample t test

# Density Plot
gapminder %>% 
  filter(continent == c("Africa", "Europe")) %>%
  ggplot(aes(lifeExp, fill = continent)) +
  geom_density(alpha = 0.4)
# Wrong-------------


# Density Plot
gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>%
  ggplot(aes(lifeExp, fill = continent)) +
  geom_density(alpha = 0.4)



# %in% operator indicates that the left argument is
# in the vector to the right argument.
# It is called a "membership operator."


gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>%
  t.test(lifeExp ~ continent, data =.,
         alternative = "two.sided")

gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>%
  t.test(lifeExp ~ continent, data =.,
         alternative = "two.sided",
         var.equal = FALSE)

gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>%
  t.test(lifeExp ~ continent, data =.,
         alternative = "two.sided",
         var.equal = TRUE)


gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>%
  t.test(lifeExp ~ continent, data =.,
         alternative = "less",
         var.equal = FALSE)

# Two Variance Test

gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>%
  var.test(lifeExp ~ continent, data =.,
         alternative = "two.sided")


# In-Class Question of the Day Work

gapminder %>% 
  filter(country %in% c("Ireland", "Switzerland")) %>%
  var.test(lifeExp ~ country, data =.,
           alternative = "less")

gapminder %>% 
  filter(country %in% c("Ireland", "Switzerland")) %>%
  t.test(lifeExp ~ country, data =.,
           alternative = "less",
         var.equal = FALSE)


gapminder %>% 
  filter(country %in% c("Ireland", "Switzerland")) %>%
  ggplot(aes(lifeExp, fill = country)) +
  geom_density(alpha = 0.4) +
  xlim(60, 90)


gapminder %>% 
  filter(country %in% c("Ireland", "Switzerland")) %>%
  var.test(lifeExp ~ country, data = .,
           alternative = "two.sided")


gapminder %>% 
  filter(country %in% c("Ireland", "Switzerland")) %>%
  var.test(lifeExp ~ country, data = .,
           alternative = "less",
           var.equal = TRUE)




