# In-class Exercise 1-27-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass4")


##########################################################################

data(mpg)

?mpg


# Scatter Plot
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point()

# Base code
ggplot(mpg, aes(displ, hwy)) +  # Same thing as the code above
  geom_point()

# NOT Typical
ggplot(mpg) +
  geom_point(aes(displ, hwy))

# When using a variable for color codes,
# YOU SHOULD put "aes" in the parenthesis
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))

# To add a SINGLE COLOR in geom_,
# YOU SHOULD NOT put "aes" in the parenthesis
ggplot(mpg, aes(displ, hwy)) +
  geom_point(color = "red")


ggplot(mpg, aes(displ, hwy)) +
  geom_point(color = "purple")

# Incorrect Coding
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = "purple"))

# Not useful
ggplot(mpg, aes(displ, hwy)) +  
  geom_point(aes(size = class))

ggplot(mpg, aes(displ, hwy)) +  
  geom_point(aes(shape = class)) ### max number of shapes = 6

ggplot(mpg, aes(displ, hwy)) +  
  geom_point(aes(alpha = class))


ggplot(mpg, aes(displ, hwy)) +  
  geom_point() +
  facet_wrap(~class)
  

ggplot(mpg, aes(displ, hwy)) +  
  geom_point() +
  facet_wrap(~class, nrow = 2)
  
  
ggplot(mpg, aes(displ, hwy)) +  
  geom_point(aes(color = fl, shape = trans)) +
  facet_wrap(~class, nrow = 2)
  
  
ggplot(mpg, aes(displ, hwy)) +  
  geom_point(aes(color = trans, shape = fl)) +
  facet_wrap(~class, nrow = 2)



ggplot(mpg, aes(displ, hwy)) +  
  geom_point() +
  geom_smooth()
  
ggplot(mpg, aes(displ, hwy)) +  
  geom_point() +
  geom_smooth(method = "lm")
  
ggplot(mpg, aes(displ, hwy)) +  
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Histogram
ggplot(mpg, aes(hwy)) +
  geom_histogram()


ggplot(mpg, aes(hwy)) +
  geom_histogram(binwidth = 5)

ggplot(mpg, aes(hwy)) +
  geom_histogram(bins = 15)


ggplot(mpg, aes(hwy)) +
  geom_histogram(binwidth = 5,
                 color = "blue",
                 fill = "skyblue")


ggplot(mpg, aes(hwy)) +
  geom_histogram(binwidth = 5,
                 color = "blue",
                 fill = "skyblue") +
  theme_light()



# Box Plot

ggplot(mpg, aes(class, hwy)) +
  geom_boxplot()

# Bar (Column) Plot
ggplot(mpg, aes(class, hwy)) +
  geom_col()

