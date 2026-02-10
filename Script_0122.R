# In-class Exercise 1-22-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass3")

##########################################################################

# Load mtcars data file
cars <- read.csv("mtcars.csv")

# Measures of Location
sort(cars$mpg)

# 10th Percentile (Linear Interpolation Method)
quantile(cars$mpg, 0.10)

# 10th, 20th, 30th Percentile
quantile(cars$mpg, c(0.10, 0.20, 0.30))

# Quartiles of MPG
quantile(cars$mpg)

summary(cars$mpg)

quantile(cars$mpg, probs = seq(0,1, by = 0.25))


# Deciles of MPG
quantile(cars$mpg, probs = seq(0,1, by = 0.10))

# Boxplot
boxplot(cars$mpg)

boxplot(cars$mpg, horizontal = TRUE)

boxplot(cars$mpg, horizontal = TRUE,
        main = "Miles per Gallon")


# Scatter Plot

?mtcars

plot(cars$wt, cars$mpg) # (x, y) or (y ~ x)

plot(cars$wt, cars$mpg,
     main = "Scatter Plot between weight and MPG",
     xlab = "Weight (1000 lbs)",
     ylab = "MPG",
     pch = 20) # 19 is larger then 20 for the size of the dots

# Add Trend Line in Regression Style

model <- lm(cars$mpg ~ cars$wt)

abline(model, col="red")


# Covariance
cov(cars$wt, cars$mpg)


# Correlation Coefficient
cor(cars$wt, cars$mpg)


###########################################################################

# Unit 3

# Scatter Plot Matrix - Question 4 on HW 1
pairs(cars)

pairs(cars[,2:12])

pairs(cars[,2:12], pch=20)


pairs(cars[,c(2, 4:8)], pch=20)

pairs(cars[,-c(1, 3, 9, 10, 11, 12)], pch=20)


# Correlation Matrix = Question 3 on HW 1
cor(cars[,c(2, 4:8)])

round(cor(cars[,c(2, 4:8)]), 3)



