library(bayesplot)


## This function calculates performance measures: conditional predictive ordinate, WAIC,log likelihood, and log pseudomarginal likelihood
final_stats <- function(r) {
  CPO <- summary(r, pars = c("CPOinv"))$summary[,1]
  LPML <- sum(log(1 / CPO))
  log_lik <- loo::extract_log_lik(r)
  WAIC <- loo::waic(log_lik)[[5]]
  
  return(c(LPML = LPML,
              WAIC = WAIC))
}


##prints out summaries of the posterior distributions
summary_coef <- function(r) {
  summary(r, pars = c(names(r)[startsWith(names(r), "beta")], "tau"))$summary %>%
    as.data.frame() %>% 
    rownames_to_column("Variable") %>%
    as_tibble()
}

##prints out summaries of the posterior distributions
summary_coef_0 <- function(r) {
  summary(r, pars = c(names(r)[startsWith(names(r), "Beta")]))$summary %>% 
    as.data.frame() %>% 
    rownames_to_column("Variable") %>%
    as_tibble()
}

##prints out summaries of the posterior predictive distributions
summary_ynew <- function(r) {
  summary(r, pars = c(names(r)[startsWith(names(r), "ynew")]))$summary %>% 
    as.data.frame() %>% 
    rownames_to_column("y") %>%
    as_tibble()
}

##plots posterior distributions and credible intervals
mcmc_areas_coef <- function(r) {
  mcmc_areas(as.matrix(r), pars = names(r)[startsWith(names(r), "beta")])
}