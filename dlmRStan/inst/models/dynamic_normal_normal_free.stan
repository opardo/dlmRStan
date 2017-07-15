/*
* Stochastic Trend Random Walk Beta for Explanatory Variable Fixed Variances
*/

data {
  	int<lower=1> m;
  	int<lower=1> p;
  	vector [m] y;
  	matrix [m,p] X;
  	vector [p] apriori_means;
  	matrix [p,p] apriori_covariances;
  	real intercept_mean;
  	real <lower=0> intercept_var;
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
}
transformed parameters {
  	vector [m] yhat;
  	for (t in 1:m){
    	yhat[t] = intercept[t] + dot_product(row(beta,t), row(X,t));
  	}
}
model {
  	sigma ~ gamma(2 * sigma_lower, 1);
  	beta[1] ~ multi_normal(apriori_means, sigma * apriori_covariances);
  	intercept[1] ~ normal(intercept_mean, intercept_var);
  	y[1] ~ normal(yhat[1], sigma);
  	for (t in 2:m) {
  		intercept[t] ~ normal(intercept[t-1], intercept_var);
	 	  beta[t] ~ multi_normal(beta[t-1], sigma * apriori_covariances);
	 	  y[t] ~ normal(intercept[t] + dot_product(row(beta,t), row(X,t)), sigma);
  	}
}
generated quantities {
	vector[m] log_lik;
	for (t in 1:m)
		log_lik[t] = normal_lpdf(y[t] | yhat[t], sigma);
}
