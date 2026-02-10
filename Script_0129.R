# In-class Exercise 1-29-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass5")


##########################################################################


# Pipe Operator %>% 

drinks <- read.csv("softdrink.csv")


# Frequency Table
table(drinks$brand)

# Relative Frequency Table
prop.table(table(drinks$brand))

addmargins(prop.table(table(drinks$brand)))


# Relative Frequency Table using Pipe Operator %>% 
drinks$brand %>% 
  table() %>% 
  prop.table() %>% 
  addmargins()
  

##########################################################################

?diamonds

data("diamonds")

diamonds$cut %>% 
  table() %>% 
  prop.table() %>% 
  addmargins()

diamonds$cut %>% 
  table() %>% 
  prop.table() %>% 
  addmargins() %>% 
  round(3)



str(diamonds)

glimpse(diamonds)


# Subset by Row with filter()

diamonds2 <- filter(diamonds, cut == "Ideal")
## == maching or equal to
## = calculation or assign (like <-)

diamonds2 <- diamonds %>% 
  filter(cut == "Ideal")


diamonds2 <- diamonds %>% 
  filter(price > 10000)

diamonds2 <- diamonds %>% 
  filter(cut == "Ideal",
         price > 10000)  ## comma represents 'and'

diamonds2 <- diamonds %>% 
  filter(cut == "Ideal"|
         price > 10000)  ## | represents 'or'


# Subset by Column with select()

?select

diamonds3 <- diamonds %>% 
  select(cut, color)

diamonds3 <- diamonds %>% 
  select(color, cut)

diamonds3 <- diamonds %>% 
  select(1:4)

diamonds3 <- diamonds %>% 
  select(starts_with("c"))

diamonds3 <- diamonds %>% 
  select(contains("c"))

diamonds3 <- diamonds %>% 
  select(price, everything())

diamonds3 <- diamonds %>% 
  select(-price)


# Reorder rows with arrange()

diamonds4 <- diamonds %>% 
  arrange(color)

diamonds4 <- diamonds %>% 
  arrange(carat)

diamonds4 <- diamonds %>% 
  arrange(color, carat)

diamonds4 <- diamonds %>% 
  arrange(-carat)

diamonds4 <- diamonds %>% 
  arrange(desc(carat))


# Add or modify columns with mutate()

diamonds5 <- diamonds %>%
  mutate(grams = 0.2*carat)
  
diamonds5 <- diamonds5 %>%
  select(carat, grams, everything())







