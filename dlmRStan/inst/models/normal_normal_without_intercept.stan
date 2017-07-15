data {
    int<lower=1> m;
    int<lower=1> p;
    vector [m] y;
    matrix [m,p] X;
    vector [p] apriori_means;
    matrix [p,p] apriori_covariances;
    real betas_lower;
    real betas_upper;
    real <lower=0> sigma_lower;
  }
parameters {
  matrix <lower=betas_lower, upper=betas_upper> [m,p] beta;
  real <lower=sigma_lower> sigma;
}
transformed parameters {
  vector [m] yhat;
  for (t in 1:m){
    yhat[t] = dot_product(row(beta,t), row(X,t));
  }
}
model {
  sigma ~ gamma(2 * sigma_lower, 1);
  beta[1] ~ multi_normal(apriori_means, sigma * apriori_covariances);
  y[1] ~ normal(yhat[1], sigma);
  for (t in 2:m) {
    beta[t] ~ multi_normal(beta[t-1], sigma * apriori_covariances);
    y[t] ~ normal(dot_product(row(beta,t), row(X,t)), sigma);
  }
}
generated quantities {
  vector[m] log_lik;
  for (t in 1:m)
    log_lik[t] = normal_lpdf(y[t] | yhat[t], sigma);
}
