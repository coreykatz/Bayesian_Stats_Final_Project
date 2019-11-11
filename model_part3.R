model2 <- "
data {
  int<lower=0> N;   // number of data items
  int<lower=0> K;   // number of predictors
  int<lower=0> D;   // number of days
  matrix[N, K] x;   // predictor matrix
  matrix[N,D] y;    // outcome vector
  
  int<lower=0> M;  // number of new households
  matrix[M,K] x_new; // predictor matrix
  
  vector[K] beta_0_pop; // beta prior mean for population
  matrix[K, K] inv_covariance; // covariance matrix
  real<lower=0> a;
  real<lower=0> b;
}

parameters {
  vector[K] beta_pop;// coefficients for intercept & predictors at population level
  matrix[K,D] beta_day; //coefficients for intercept & predictors at day level
  real<lower=0> tau_pop;  // error scale
  real<lower=0> tau_day[D]; // day level variance
}

model {
  //variance priors
  tau_pop ~ gamma(a, b);
  tau_day ~gamma(a,b);
  
  //population level beta priors
  beta_pop ~ multi_normal(beta_0_pop, (N*inv_covariance)/tau_pop);
  
  for (d in 1:D){
    beta_day[,d] ~ multi_normal(beta_pop, (N*inv_covariance)/tau_day[d]); //day level coef. prior
    y[,d] ~ normal(x * beta_day[,d], 1 / sqrt(tau_day[d]));  // likelihood
  }
}

generated quantities{
  matrix[M,D] ynew;
  vector[M] ynew_p;
  
  for(d in 1:D){
    for(i in 1:M) {
      ynew[i,d] = normal_rng(x_new[i,] * beta_day[,d], 1 / sqrt(tau_day[d])); //prediction day level
    }
  }
  for(i in 1:M) {
    ynew_p[i] = normal_rng(x_new[i,] * beta_pop, 1 / sqrt(tau_pop)); //prediction population level
  }
}

"