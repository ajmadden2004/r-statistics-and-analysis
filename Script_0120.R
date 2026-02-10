# In-class Exercise 1-20-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass2")

##########################################################################

# Basic Calculations
4+5
200/5

z <- 3
z

y <- -2
x <- c(1,2,6)
w <- -1:4
v <- z*y
u <- x*z
n <- sqrt(w)
m <- sqrt(z)
q <- w^2


#########################################################################


# Frequency Table using a categorical variable

#Load softdrink data file
drinks <- read.csv("softdrink.csv")

# Frequency Table
table(drinks$brand)
addmargins(table(drinks$brand))

# Relative Frequency Table
prop.table(table(drinks$brand))
addmargins(prop.table(table(drinks$brand)))

# Percent Frequency Table
prop.table(table(drinks$brand))*100
addmargins(prop.table(table(drinks$brand)))*100



#########################################################################

# Load mtcars data file
cars <- read.csv("mtcars.csv")

?mtcars

# Review Data File
names(cars)
head(cars)
cars[1:10,]
cars[,1:10]

str(cars)

summary(cars)
summary(cars$mpg)

# Histogram
hist(cars$mpg)

# Histogram - Number of Bins (each frequency bar represents a bin - so there are 5 bins here)
hist(cars$mpg, breaks = 10)

# Histogram - Width of Bins
hist(cars$mpg, breaks = seq(10, 35, by = 3))

# Histogram - Title & X-axis
hist(cars$mpg, breaks = 10, main = "Miles Per Gallon")

hist(cars$mpg, breaks = 10, 
     main = "Miles Per Gallon",
     xlab = "MPG")


#########################################################################

# Central Tendency
mean(cars$mpg)

median(cars$mpg)

mode(cars$mpg)

install.packages("DescTools")
library(DescTools)

Mode(cars$mpg)

sort(cars$mpg)

#########################################################################

# Measures of Variability

range(cars$mpg)
max(cars$mpg)-min(cars$mpg)

var(cars$mpg)

sd(cars$mpg)

sd(cars$mpg)/mean(cars$mpg)

cv <- sd(cars$mpg)/mean(cars$mpg)

cv

