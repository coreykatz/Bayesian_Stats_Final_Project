model1 <- "
data {
  int<lower=0> N;   // number of data items
  int<lower=0> K;   // number of predictors
  matrix[N, K] x;   // predictor matrix
  vector[N] y;      // outcome vector
  
  vector[K] mu_beta; // beta prior mean
  matrix[K, K] Sigma_beta;// beta prior covariance
  real<lower=0> a;
  real<lower=0> b;
  
  int<lower=0> M;   // number of new data items
  matrix[M, K] xnew;
}
parameters {
  vector[K] beta;       // coefficients for intercept & predictors
  real<lower=0> tau;  // error scale
}
transformed parameters{
  vector[N] CPOinv; //conditional predictive ordinate
  vector[K] BetaMoreThan0;
  
  for (k in 1:K){
    BetaMoreThan0[k] = step(beta[k]);
  }
  
  for (n in 1:N){
    CPOinv[n] = sqrt(2*3.14159 / tau) * exp(0.5 * tau * pow(y[n] - x[n,] * beta, 2)); // calculate conditional predictive ordinate
  }
}
model {
  tau ~ gamma(a, b);
  beta ~ multi_normal(mu_beta, Sigma_beta / tau);
  
  y ~ normal(x * beta, 1 / sqrt(tau));  // likelihood
}
generated quantities {
  vector[M] ynew;
  vector[N] log_lik;
  
  for(i in 1:M) {
    ynew[i] = normal_rng(xnew[i,] * beta, 1 / sqrt(tau));
  }
  for (n in 1:N){
    log_lik[n] = normal_lpdf(y[n] | x[n] * beta, 1 / sqrt(tau));
  }
}
"
