#Models and Results for parts 3-4.

library(rstan)
library(readr)
library(tidyverse)
library(here)
source(here("summary_functions_205.R"))
options(digits = 4)
model_part3 <- read_rds(here("model_part3.rds"))

full_dat <- read.table("IrishElectricity.txt", as.is = T, header = T) %>% 
  as_tibble()
full_dat_x <- as.matrix(cbind(1, full_dat[,1:6]))
full_dat_y <- as.matrix(full_dat[,-(1:6)])

partial_y <- full_dat_y[,c(1,2,3)]
# Modeling All -----------------------------------------------------------------
X <- full_dat_x

y <- full_dat_y

N <- nrow(X)
K <- ncol(X)
D <- ncol(y)

beta_0_pop <- rep(0, K)
inv_covariance <- solve(t(X) %*% X)
a <- b <- 0.001


set.seed(205)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



data_part3 <- list(N = N, K = K, D=D, x = X, y = y,
                   a = a, b = b,
                   beta_0_pop = beta_0_pop,
                   inv_covariance=inv_covariance,
                   x_new = full_dat_x[c(30, 83, 91),], M = 3)

r_part3 <- sampling(object = model_part3, data = data_part3)

Beta_post <- as.vector(summary(r)$summary[c(1,2,3,4,5,6,7)])
fitted.y <- X %*% Beta_post
residuals <- y[,1]- fitted.y

plot(residuals~fitted.y)
write_rds(r_part3, here("r_part3.rds"))

write_rds(r, here("r_hierarchical_part3.rds"))

r_p3 <- read_rds(here("r_hierarchical_part3.rds"))
Beta_post_p3 <- as.vector(summary(r_p3)$summary[c(1,2,3,4,5,6,7)])
fitted.y_p3 <- X %*% Beta_post_p3
residuals_p3 <- y[,1]- fitted.y_p3

plot(residuals_p3~fitted.y_p3)

# Model 3 ------------------------------------------
# Question 4

model_part4 <- read_rds(here("model_part4.rds"))

T_n <- 2
D_before <- 47

set.seed(205)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

data_part4 <- list(N = N, K = K, D=D, x = X, y = y,
                   T_n = T_n, D_before = D_before,
                   a = a, b = b,
                   beta_0_pop = beta_0_pop,
                   inv_covariance = inv_covariance,
                   M = nrow(full_dat_x), x_new = full_dat_x)

r_part4 <- sampling(object = model_part4, data = data_part4)
write_rds(r_part4, here("r_part4.rds"))