library(tidyverse)
library(magrittr) #because power is in the pipes
library(MuMIn)
library(glmnet)


setwd("/home/GIT/monthly-programming-challenge/R/02")
d <- read_delim("winequality-white.csv", delim = ";")

# a bit of data exploration -------------
d %>% 
  pivot_longer(1:12, names_to = "Variable", values_to = "value") %>% 
  ggplot() +
  geom_histogram(aes(value), bins = 20) +
  facet_wrap(~ Variable, ncol = 3, scales = "free") +
  theme_minimal()

# better to log-transform some variables, imho
d %>% 
  mutate(chlorides = log10(chlorides),
         sulphates = log10(sulphates),
         `volatile acidity` = log10(`volatile acidity`)) %>% 
  pivot_longer(1:12, names_to = "Variable", values_to = "value") %>% 
  ggplot() +
  geom_histogram(aes(value), bins = 20) +
  facet_wrap(~ Variable, ncol = 3, scales = "free") +
  theme_minimal()

# let's scale also
d %<>% 
  mutate(chlorides = log10(chlorides),
         sulphates = log10(sulphates),
         `volatile acidity` = log10(`volatile acidity`)) %>% 
  scale() %>% 
  as_tibble()

# modelling -------------
m <- lm(quality ~ ., data = d)

# step-AIC
options(na.action = "na.fail")
aic_step <- dredge(m)

# lasso
lambdas <- 10^seq(2, -3, by = -.1)
m <- cv.glmnet(as.matrix(d[, -which(names(d) == "quality")]), 
            d$quality,
            lambda = lambdas)

m <- glmnet(as.matrix(d[, -which(names(d) == "quality")]), 
      d$quality,
      lambda = m$lambda.min)
coef(m)
