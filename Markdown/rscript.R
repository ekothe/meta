# Load some packages
set.seed(46131)
library(tidyverse)
library(psych)
library(broom)

# Generate some Random Data

observations <- tibble(
  a = rnorm(100, 15, 3.5),
  b = rnorm(100, 18, 2.8)
  )

# Describe the data

describe(observations)


# Plot the correlation (using ggplot2)

ggplot(observations, aes(a,b)) + geom_point() + theme_bw()

# Estimate a model for these data

model <-  lm(a~b, observations)
summary(model)
glance(model)
