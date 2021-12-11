#' @title Decay rate estimate
#'
#' @param k estimate
#' @param A estimate
#' @param half_time
#' @return ggplot of predection with non linear model
#'
#' @description the built functions are designed to find safe estimates for A and k parameters, calculate half time, and predict the non-linear model.







# Decay rate estimates


## Libraries
library(tidyverse)
library(ggplot2)
library(modelr)
library(nls2)
library(SciViews)



## Data

decay <- read.csv("/Users/aliabdulaziz/Desktop/fake_exp_decay_data.csv", header=T, na.strings="?")

summary(decay)

ggplot(data = decay) +
  geom_point(aes(x = x, y = y), color = "tomato1") +
  facet_wrap(~ treatment)

ggplot(data = decay) +
  geom_point(aes(x = x, y = y), color = "blue") +
  facet_wrap(~ site)


## Linearization

lin_mod <- lm(data = decay, y ~ x)
linear_intercept <- coef(lin_mod)[1]
linear_slope <- coef(lin_mod)[2]

k_guess <- exp(linear_slope)
A_guess <- exp(linear_intercept)


## Non-linear model

rn_model <- nls2(data = decay, y ~ A*exp(-k*x), start = list(k = k_guess, A = A_guess))

safe_fit <- function(df) {
  # Tries to fit the NLS model to the data, and returns NA if it can't for whatever reason
  fit <- tryCatch(
    nls2::nls2(formula = y ~ A * exp(-k * x),
               data = df,
               start = list(A = A_guess, k = k_guess)),
    error = function(e) NA)
  fit
}



## Find A estimates

safely_get_A_estimate_1 <- function(nls_obj) {
  #browser()
  # CHeck whether the object is NA
  # NOte that is.na called on an nls2 object will return a vector of booleans
  if(!(is.na(nls_obj))[1]) { # So this returns TRUE if the first element of hte vector that is.na() returns is FALSE
    # Here goes the code to pull out the esimate for A
    A.estimate <- summary(nls_obj)$coefficients["A", "Estimate"]

  } else {
    # This gets triggered if the object is NA,
    #   Which would have happened if the model fit didn't work
    A.estimate <- NA
  }
  A.estimate
}

d_errors_A <- decay %>%
  group_by(treatment, rep, site) %>%
  nest(data = c(y, x)) %>%
  mutate(fits = map(data, safe_fit),
         A = map_dbl(fits, safely_get_A_estimate_1))


## Find k estimates

safely_get_k_estimate <- function(nls_obj) {
  #browser()
  # CHeck whether the object is NA
  # NOte that is.na called on an nls2 object will return a vector of booleans
  if(!(is.na(nls_obj))[1]) { # So this returns TRUE if the first element of hte vector that is.na() returns is FALSE
    # Here goes the code to pull out the esimate for A
    k.estimate <- summary(nls_obj)$coefficients["k", "Estimate"]

  } else {
    # This gets triggered if the object is NA,
    #   Which would have happened if the model fit didn't work
    k.estimate <- NA
  }
  k.estimate
}

d_errors_k <- decay %>%
  group_by(treatment, rep, site) %>%
  nest(data = c(y, x)) %>%
  mutate(fits = map(data, safe_fit),
         k = map_dbl(fits, safely_get_k_estimate))


## Nest data

n_nest <- decay %>%
  group_by(treatment, rep, site) %>%
  nest(data = c(y, x)) %>%
  mutate(fits = map(data, safe_fit),
         k = map_dbl(fits, safely_get_k_estimate),
         A = map_dbl(fits, safely_get_A_estimate_1))

## Calculate half time

half_time <- (ln(2)/n_nest$k)
print(half_time)


## Plots

## Create predictions and fit the model

unnest <- unnest(n_nest, data)
grid <- seq(from = min(unnest$x), to = max(unnest$x), length.out = 1000)
df <- data.frame(x = grid)
# Get predictions for each model
preds <- predict(rn_model, newdata = df)

preds <- df %>%
  mutate(y = preds)

ggplot() +
  geom_line(data = preds, aes(x = x, y = y), color = "black") +
  geom_point(data = unnest, aes(x =x, y = y), color = "sandybrown") +
  facet_wrap(~ treatment)

ggplot() +
  geom_line(data = preds, aes(x = x, y = y), color = "black") +
  geom_point(data = unnest, aes(x =x, y = y), color = "red") +
  facet_wrap(~ site)


### Estimated k

ggplot(data = n_nest) +
  geom_boxplot(aes(x = treatment, y = k, fill = rep))

ggplot(data = n_nest) +
  geom_boxplot(aes(x = site, y = k, fill = rep))
