/*
* Dynamic linear model
*/

data {
  	int<lower=1> m;
  	int<lower=1> p;
  	vector [m] y;
  	matrix [m,p] X;
  	vector [p] betas_means;
  	matrix [p,p] betas_covariances;
  	real intercept_mean;
  	real <lower=0> intercept_sd;
  	real betas_lower;
  	real betas_upper;
  	real intercept_lower;
  	real intercept_upper;
  	real <lower=0> sigma_lower;
}
parameters {
  	matrix <lower=betas_lower, upper=betas_upper> [m,p] beta;
  	real <lower=sigma_lower> sigma;
  	vector <lower=intercept_lower, upper=intercept_upper> [m] intercept;
  	vector <lower=betas_lower, upper=betas_upper> [p] first_beta;
  	real <lower=intercept_lower, upper=intercept_upper> first_intercept;
  	real <lower=0, upper=1> beta_cov_scale;
  	real <lower=0, upper=1> intercept_sd_scale;
}
transformed parameters {
  	vector [m] yhat;
  	for (t in 1:m){
    	yhat[t] = intercept[t] + dot_product(row(beta,t), row(X,t));
  	}
}
model {

    intercept[1] ~ normal(first_intercept, intercept_sd_scale * intercept_sd);
  	beta[1] ~ multi_normal(first_beta, beta_cov_scale * betas_covariances);
  	y[1] ~ normal(yhat[1], sigma);

  	for (t in 2:m) {
  		intercept[t] ~ normal(intercept[t-1], intercept_sd_scale * intercept_sd);
	 	  beta[t] ~ multi_normal(beta[t-1], beta_cov_scale * betas_covariances);
	 	  y[t] ~ normal(yhat[t], sigma);
  	}

  	sigma ~ gamma(0.2 * sigma_lower, 0.1);
  	first_intercept ~ normal(intercept_mean, intercept_sd);
  	first_beta ~ multi_normal(betas_means, betas_covariances);
  	intercept_sd_scale ~ beta(15,5);
  	beta_cov_scale ~ beta(15,5);

}
generated quantities {
	vector[m] log_lik;
	for (t in 1:m)
		log_lik[t] = normal_lpdf(y[t] | yhat[t], sigma);
}
