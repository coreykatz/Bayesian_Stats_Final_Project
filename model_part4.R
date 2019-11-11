model3 <- 

"
data {
  int<lower=0> N;   // number of data items
  int<lower=0> K;   // number of predictors
  int<lower=0> D;   // number of days
  int<lower=0> T_n;   // number of time intervals
  matrix[N, K] x;   // predictor matrix
  matrix[N,D] y;      // outcome vector
  int<lower=0> D_before;
  
  
  vector[K] beta_0_pop; // beta prior mean for population
  matrix[K, K] inv_covariance; // covariance matrix
  real<lower=0> a;
  real<lower=0> b;
}

parameters {
  vector[K] beta_pop; // coefficients for intercept & predictors at population level
  matrix[K,T_n] beta_ba; // coefficicents for the two time intervals
  matrix[K,D] beta_day; //coefficients for intercept & predictors at day level
  // variance parameters
  real<lower=0> tau_pop; 
  real<lower=0> tau_ba[T_n];
  real<lower=0> tau_day[D];
}

model {
  tau_pop ~ gamma(a, b); //prior distribution on population level variance
  tau_day ~gamma(a,b);//prior distribution on day level variance
  tau_ba ~ gamma(a,b);//prior distribution on before/after date variance
  
  beta_pop ~ multi_normal(beta_0_pop, (N*inv_covariance)/tau_pop);//popualtion level prior on coef.
  
  for (t in 1:T_n){
    beta_ba[,t] ~ multi_normal(beta_pop, (N*inv_covariance)/tau_ba[t]);
  }
  
  for (d in 1:D_before){
    beta_day[,d] ~ multi_normal(beta_ba[,1], (N*inv_covariance)/tau_day[d]); //day level coef. prior before certain date
    y[,d] ~ normal(x * beta_day[,d], 1 / sqrt(tau_day[d]));  // likelihood
  }
  
  for (j in (D_before+1):D){
    beta_day[,j] ~ multi_normal(beta_ba[,2], (N*inv_covariance)/tau_day[j]);//day level coef. prior after certain date
    y[,j] ~ normal(x * beta_day[,j], 1 / sqrt(tau_day[j]));  // likelihood
  }
  
}
"