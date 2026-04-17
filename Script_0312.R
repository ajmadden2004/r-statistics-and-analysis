# In-class Exercise 3-12-2026

rm(list = ls())

library(tidyverse)

setwd("C:/Users/ajmad/OneDrive/Documents/BAN313R/inclass10")


##########################################################################

library(MASS)   # Box Cox

library(rstatix)  # Shapiro

library(gapminder)

data(gapminder)

View(gapminder)


# Normality Test

# Density Plot - lifeExp
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(lifeExp)) +
  geom_density(alpha = 0.4, fill = "light blue")# Density Plot - lifeExp
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(lifeExp)) +
  geom_density(alpha = 0.4, fill = "light blue")

# Density Plot - gdpPercap
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(gdpPercap)) +
  geom_density(alpha = 0.4, fill = "light blue")

# Density Plot - gdpPercap
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(gdpPercap)) +
  geom_density(alpha = 0.4, fill = "light blue")

# Q-Q Plot - lifeExp
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(sample = lifeExp)) +       # Q-Q plot requires sample =
  geom_qq() +
  geom_qq_line()

# Q-Q Plot - gdpPercap
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(sample = gdpPercap)) +       # Q-Q plot requires sample =
  geom_qq() +
  geom_qq_line()


# Shapiro Test - lifeExp
gapminder %>% 
  filter(continent == "Africa") %>% 
  shapiro_test(lifeExp)

# Shapiro Test - gdpPercap
gapminder %>% 
  filter(continent == "Africa") %>% 
  shapiro_test(gdpPercap)

# Scatter Plot 
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(lifeExp, gdpPercap)) +
  geom_point(shape = 20) +
  geom_smooth(method = lm, se = F)

gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(lifeExp, gdpPercap)) +
  geom_point(shape = 20) +
  geom_smooth(se = F)


# Regression (IV = lifeExp, DV = gdpPercap)
gapminder %>% 
  filter(continent == "Africa") %>% 
  lm(gdpPercap ~ lifeExp, data =.) %>% 
  summary()


# Residual Analysis
gapminder %>% 
  filter(continent == "Africa") %>% 
  lm (gdpPercap ~ lifeExp, data =.) %>% 
  plot()


# Box Cox to identify optimal lambda
gapminder %>% 
  filter(continent == "Africa") %>% 
  boxcox(gdpPercap ~ lifeExp, data =.)


# Find the exact value of optimal lambda
bc <- gapminder %>% 
  filter(continent == "Africa") %>% 
  boxcox(gdpPercap ~ lifeExp, data =.)

optimal_lambda <- bc$x[which.max(bc$y)]

optimal_lambda

# Create transformed variables
gapminder$invsqrt_gdpPercap <- 1/sqrt(gapminder$gdpPercap)    # Dependent variable

gapminder$invsqrt_lifeExp <- 1/sqrt(gapminder$lifeExp)      # IV

gapminder$log_gdpPercap <- log(gapminder$gdpPercap)

gapminder$log_lifeExp <- log(gapminder$lifeExp)

# Desnity Plot
# Density Plot - lifeExp - Original Scale
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(lifeExp)) +
  geom_density(alpha = 0.4, fill = "light blue")

# Density Plot - lifeExp - 1/sqrt Scale
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(invsqrt_lifeExp)) +
  geom_density(alpha = 0.4, fill = "light blue") +
  xlim(0.1, 0.21)

# Density Plot - lifeExp - log Scale
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(log_lifeExp)) +
  geom_density(alpha = 0.4, fill = "light blue") +
  xlim(3.1, 4.6)


  
# Density Plot - gdpPercap - Original Scale
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(gdpPercap)) +
  geom_density(alpha = 0.4, fill = "light blue")

# Density Plot - gdpPercap - 1/sqrt Scale
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(invsqrt_gdpPercap)) +
  geom_density(alpha = 0.4, fill = "light blue") +
  xlim(0, 0.07)

# Density Plot - gdpPercap - log Scale
gapminder %>% 
  filter(continent == "Africa") %>% 
  ggplot(aes(log_gdpPercap)) +
  geom_density(alpha = 0.4, fill = "light blue")




# Regression (IV = invsqrt_lifeExp, DV = invsqrt_gdpPercap)
gapminder %>% 
  filter(continent == "Africa") %>% 
  lm(invsqrt_gdpPercap ~ invsqrt_lifeExp, data =.) %>% 
  summary()

# Adjusted R-squared:  0.2458 

gapminder %>% 
  filter(continent == "Africa") %>% 
  lm(invsqrt_gdpPercap ~ invsqrt_lifeExp, data =.) %>% 
  plot()



# Regression (IV = log_lifeExp, DV = log_gdpPercap)
gapminder %>% 
  filter(continent == "Africa") %>% 
  lm(log_gdpPercap ~ log_lifeExp, data =.) %>% 
  summary()

# Adjusted R-squared:  0.263

gapminder %>% 
  filter(continent == "Africa") %>% 
  lm(log_gdpPercap ~ log_lifeExp, data =.) %>% 
  plot()
  


# Regression (IV = lifeExp, DV = invsqrt_gdpPercap)
gapminder %>% 
  filter(continent == "Africa") %>% 
  lm(invsqrt_gdpPercap ~ lifeExp, data =.) %>% 
  summary()

# Adjusted R-squared:  0.2704 

gapminder %>% 
  filter(continent == "Africa") %>% 
  lm(invsqrt_gdpPercap ~ lifeExp, data =.) %>% 
  plot()



# Regression (IV = lifeExp, DV = log_gdpPercap)
gapminder %>% 
  filter(continent == "Africa") %>% 
  lm(log_gdpPercap ~ lifeExp, data =.) %>% 
  summary()

# Adjusted R-squared:  0.2849 

gapminder %>% 
  filter(continent == "Africa") %>% 
  lm(log_gdpPercap ~ lifeExp, data =.) %>% 
  plot()













