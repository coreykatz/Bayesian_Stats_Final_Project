## Sets up data and runs the stan models for different sets of variables included in the regression:

library(tidyverse)
library(here)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dat_day1 <- read.table("IrishElectricity.txt", as.is = T, header = T) %>%
  `[`( , 1:7) %>%
  as_tibble() %>% 
  rename(y = V1)

model1 <- read_rds(here("model1.rds"))

# Modeling All ----------------------------------------------------------------- Includes all varibles


## Data Setup
x_all <- cbind(1, as.matrix(dat_day1[,1:6]))

y <- dat_day1$y

N <- nrow(x_all)
K_all <- ncol(x_all)

mu_beta_all <- rep(0, K_all)
Sigma_beta_all <- solve(t(x_all) %*% x_all)
a <- b <- 0.001

set.seed(205)
my_sample <- sample(nrow(x_all), size = 5, replace = FALSE)
xnew_all <- cbind(1,
                  dat_day1 %>%
                    select(-Education, -y) %>% 
                    summarise_all(list(mean)),
                  Education = 1:5) %>%
  as.matrix() %>% 
  rbind(x_all[my_sample,])

M <- nrow(xnew_all)

data_model_all <- list(N = N, K = K_all, x = x_all, y = y,
                       a = a, b = b,
                       mu_beta = mu_beta_all,
                       Sigma_beta = N * Sigma_beta_all,
                       M = M, xnew = xnew_all) #model setup

r_all <- sampling(object = model1, data = data_model_all) #runs the model, used default sampling

write_rds(r_all, here("r_all.rds")) writes results of the model

# Modeling No Age --------------------------------------------------------------
x_noage <- x_all[,-2]

K_noage <- ncol(x_noage)

mu_beta_noage <- rep(0, K_noage)
Sigma_beta_noage <- solve(t(x_noage) %*% x_noage)

xnew_noage <- xnew_all[,-7]

set.seed(205)

data_model_noage <- list(N = N, K = K_noage, x = x_noage, y = y,
                         a = a, b = b,
                         mu_beta = mu_beta_noage,
                         Sigma_beta = N * Sigma_beta_noage,
                         M = M, xnew = xnew_noage) #model setup

r_noage <- sampling(object = model1, data = data_model_noage) #runs the model, used default sampling

write_rds(r_noage, here("r_noage.rds"))


# Modeling No Ed ---------------------------------------------------------------
x_noed <- x_all[,-7]

K_noed <- ncol(x_noed)

mu_beta_noed <- rep(0, K_noed)
Sigma_beta_noed <- solve(t(x_noed) %*% x_noed)

xnew_noed <- xnew_all[,-7]

set.seed(205)

data_model_noed <- list(N = N, K = K_noed, x = x_noed, y = y,
                        a = a, b = b,
                        mu_beta = mu_beta_noed,
                        Sigma_beta = N * Sigma_beta_noed,
                        M = M, xnew = xnew_noed)

r_noed <- sampling(object = model1, data = data_model_noed)

write_rds(r_noed, here("r_noed.rds"))

# Modeling Sub -----------------------------------------------------------------
x_sub <- x_all[,-c(2,7)]

K_sub <- ncol(x_sub)

mu_beta_sub <- rep(0, K_sub)
Sigma_beta_sub <- solve(t(x_sub) %*% x_sub)

xnew_sub <- xnew_all[,-c(2,7)]
write_rds(xnew_sub, here("xnew_sub.rds"))

set.seed(205)

data_model_sub <- list(N = N, K = K_sub, x = x_sub, y = y,
                       a = a, b = b,
                       mu_beta = mu_beta_sub,
                       Sigma_beta = N * Sigma_beta_sub,
                       M = M, xnew = xnew_sub)

r_sub <- sampling(object = model1, data = data_model_sub)

write_rds(r_sub, here("r_sub.rds"))

# Modeling No Int --------------------------------------------------------------
x_noint <- as.matrix(cbind(1, select(dat_day1, Age, Resident, Education)))

K_noint <- ncol(x_noint)

mu_beta_noint <- rep(0, K_noint)

Sigma_beta_noint <- solve(t(x_noint) %*% x_noint)

xnew_noint <- dat_day1 %>%
  select(Age, Resident, Education) %>%
  summarise_all(list(median)) %>%
  as.matrix() %>% 
  cbind("1" = 1, .) %>% 
  rbind(., .)
xnew_noint[2, 3] <- 4

M_noint <- nrow(xnew_noint)

set.seed(205)

data_model_noint <- list(N = N, K = K_noint, x = x_noint, y = y,
                         a = a, b = b,
                         mu_beta = mu_beta_noint,
                         Sigma_beta = N * Sigma_beta_noint,
                         M = M_noint, xnew = xnew_noint)

r_noint <- sampling(object = model1, data = data_model_noint)

write_rds(r_noint, here("r_noint.rds"))

# Modeling Int -----------------------------------------------------------------
x_int <- cbind(x_noint, Interaction = x_noint[,3] * x_noint[,4])

K_int <- ncol(x_int)

mu_beta_int <- rep(0, K_int)
Sigma_beta_int <- solve(t(x_int) %*% x_int)

xnew_int <- cbind(xnew_noint, Interaction = xnew_noint[,3] * xnew_noint[,4])

M_int <- nrow(xnew_int)

set.seed(205)

data_model_int <- list(N = N, K = K_int, x = x_int, y = y,
                       a = a, b = b,
                       mu_beta = mu_beta_int,
                       Sigma_beta = N * Sigma_beta_int,
                       M = M_int, xnew = xnew_int)

r_int <- sampling(object = model1, data = data_model_int)

write_rds(r_int, here("r_int.rds"))

# Modeling No Int 0.01  --------------------------------------------------------
data_model_noint01 <- list(N = N, K = K_noint, x = x_noint, y = y,
                           a = 0.01, b = 0.01,
                           mu_beta = mu_beta_noint,
                           Sigma_beta = N * Sigma_beta_noint,
                           M = M_noint, xnew = xnew_noint)

r_noint01 <- sampling(object = model1, data = data_model_noint01)

write_rds(r_noint01, here("r_noint01.rds"))

# Modeling No Int 0.0001  --------------------------------------------------------
data_model_noint0001 <- list(N = N, K = K_noint, x = x_noint, y = y,
                             a = 0.0001, b = 0.0001,
                             mu_beta = mu_beta_noint,
                             Sigma_beta = N * Sigma_beta_noint,
                             M = M_noint, xnew = xnew_noint)

r_noint0001 <- sampling(object = model1, data = data_model_noint0001)

write_rds(r_noint0001, here("r_noint0001.rds"))

```