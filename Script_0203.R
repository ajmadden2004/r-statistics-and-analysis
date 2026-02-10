# In-class Exercise 2-03-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass6")


##########################################################################

# Today's class, last class, and the one before that will help with HW 4
# filter rows, select columns


data("diamonds")


# Add or modify columns with mutate()

diamonds5 <- diamonds %>%
  mutate(grams = 0.2*carat)

diamonds5 <- diamonds5 %>%
  select(carat, grams, everything())


?diamonds


diamonds5 <- diamonds %>%
  mutate(grams = 0.2*carat,
         unitprice = price/carat)


diamonds5 <- diamonds %>%
  mutate(grams = 0.2*carat,
         unitprice = price/carat,
         cut = tolower(cut))


diamonds5 <- diamonds %>%
  mutate(grams = 0.2*carat,
         unitprice = price/carat,
         cut = tolower(cut),
         expensive = price > 10000)


diamonds5 <- diamonds %>%
  mutate(grams = 0.2*carat,
         unitprice = price/carat,
         cut = tolower(cut),
         expensive = ifelse(price > 10000, "expensive", "inexpensive"))


diamonds5 <- diamonds %>%
  mutate(grams = 0.2*carat,
         unitprice = price/carat,
         cut = tolower(cut),
         expensive = ifelse(price > 10000, "expensive", "inexpensive")) %>% 
  rename(pricy = expensive)


diamonds5 %>% 
  write.csv("diamonds5.csv")


# Grouped Summaries with group_by() and summarize()

diamonds %>% 
  group_by(cut) %>% 
  summarize(mean(price))

# diamonds %>% 
#   group_by(cut) %>% 
#   summarize(sum(price))

diamonds %>% 
  group_by(cut) %>% 
  summarize(avg_price = mean(price))

diamonds %>% 
  group_by(cut) %>% 
  summarize(avg_price = mean(price),
            sd_price = sd(price))

diamonds %>% 
  group_by(cut, color) %>% 
  summarize(avg_price = mean(price),
            sd_price = sd(price))

diamonds %>% 
  group_by(cut, color) %>% 
  summarize(avg_price = mean(price),
            sd_price = sd(price)) %>% 
  print(n=35)


diamonds %>% 
  group_by(cut, color) %>% 
  summarize(avg_price = mean(price),
            sd_price = sd(price),
            count = n()) %>% 
  print(n=35)


diamonds %>% 
  group_by(price > 10000) %>% 
  summarize(avg_price = mean(price),
            sd_price = sd(price),
            count = n())


diamonds %>% 
  group_by(expensive = price > 10000) %>% 
  summarize(avg_price = mean(price),
            sd_price = sd(price),
            count = n())



# Visualization using ggplot2

diamonds %>% 
  group_by(cut, color) %>% 
  summarize(avg_price = mean(price)) %>% 
  ggplot(aes(cut, avg_price, fill = color)) +
  geom_col(position = "dodge") # dodge: side by side (clustered column)


diamonds %>% 
  group_by(cut, clarity) %>% 
  summarize(avg_price = mean(price)) %>% 
  ggplot(aes(cut, avg_price, fill = clarity)) +
  geom_col(position = "dodge")



# Merging data sets by mutating joins

mtcars1 <- read.csv("mtcars_set1.csv")

mtcars2 <- read.csv("mtcars_set2.csv")

mtcars3 <- read.csv("mtcars_set3.csv")


# Full Join

mtcars4 <- full_join(mtcars2, mtcars3, by = "model")

mtcars4 <- mtcars2 %>% 
  full_join(mtcars3, by = "model")


# Inner Join

mtcars5 <- inner_join(mtcars2, mtcars3, by = "model")


# Left Join
mtcars6 <- left_join(mtcars1, mtcars2, by = "model")


# Left join for combining all three data files
mtcars7 <- left_join(mtcars6, mtcars3, by = "model")

mtcars7 %>% 
  write.csv("mtcars7.csv")


# Right join
mtcars8 <- right_join(mtcars1, mtcars2, by = "model")


















