/*
* Dynamic Linear Model without Intercept
*/

data {
  	int<lower=1> m;
  	int<lower=1> p;
  	vector [m] y;
  	matrix [m,p] X;
  	vector [p] betas_means;
  	matrix [p,p] betas_covariances;
  	real betas_lower;
  	real betas_upper;
  	real <lower=0> sigma_lower;
}
parameters {
  	matrix <lower=betas_lower, upper=betas_upper> [m,p] beta;
  	real <lower=sigma_lower> sigma;
  	vector <lower=betas_lower, upper=betas_upper> [p] first_beta;
  	real <lower=0, upper=1> beta_cov_scale;
}
transformed parameters {
  	vector [m] yhat;
  	for (t in 1:m){
    	yhat[t] = dot_product(row(beta,t), row(X,t));
  	}
}
model {

  	beta[1] ~ multi_normal(first_beta, beta_cov_scale * betas_covariances);
  	y[1] ~ normal(yhat[1], sigma);

  	for (t in 2:m) {
	 	  beta[t] ~ multi_normal(beta[t-1], beta_cov_scale * betas_covariances);
	 	  y[t] ~ normal(yhat[t], sigma);
  	}

  	sigma ~ gamma(0.2 * sigma_lower, 0.1);
  	first_beta ~ multi_normal(betas_means, betas_covariances);
  	beta_cov_scale ~ beta(15,5);

}
generated quantities {
	vector[m] log_lik;
	for (t in 1:m)
		log_lik[t] = normal_lpdf(y[t] | yhat[t], sigma);
}
