# Clean Work Space
rm(list = ls())
library(tidyverse)

# install.packages("tidyverse")

library(tidyverse) # loading a package

# Set Directory
setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass1")

# Verify Working Directory
getwd()

# Check Available Built in Data Files
data()

# Open mtcars data file
data("mtcars")

# Data Dictionary
?mtcars

# Save built in data file as a CSV file
write.csv(mtcars, file="mtcars.csv")

# Open an external data file (csv file in my WD)
cars <- read.csv("mtcars.csv")   # same as =

# Remove data file from environment
rm(mtcars)

# Review data file
names(cars)
head(cars)
cars[1:10,]
cars[,1:10]
