# dlmRStan

## Author

Carlos Omar Pardo (omarpardog@gmail.com)

## Description

Bayesian linear model in a time series context, with different beta for each time period, considering a correlation structure between all of them. Are also commonly known as Dynamic Linear Models (DLMs). 

The package uses [RStan](http://mc-stan.org/users/interfaces/rstan) to develop and run the MCMC algorithm.

## Installation

First of all, you must [install RTools](http://thecoatlessprofessor.com/programming/rcpp/install-rtools-for-rcpp/), since the algorithm runs in C++.

Then you should run the following code to install the package from Github (hope soon is going to be in CRAN).

```{r}
install.packages("devtools")
library(devtools)
install_github("opardo/dlmRStan")
```

## Example

### Context
The next example takes place in the market research context, trying to explain a [corporate brand](https://en.wikipedia.org/wiki/Brand_architecture)'s *Awareness* with the TV investment data. *Awareness* is a KPI defined as the percentage of people who declares to know the corporate brand. The TV investment is in *[Adstocked](https://en.wikipedia.org/wiki/Advertising_adstock) [GRPs](https://es.wikipedia.org/wiki/Gross_Rating_Points)*, an unit which removes the currency fluctuations, and takes into account that a commercial is reminded, even if it was seen time ago. 

For this specific case, 4 covariates are used:
- Main Thrust: commercials about corporate brand's image
- Sub-brands: advertising for specific products
- Competitor 1
- Competitor 2

*Awareness* is a special metric because the competitors' effect is almost always non-negative. In the worst case, consumers don't associate the competitors' ads with the studied brand and the contribution to *Awareness* is 0. But in other cases there is a confussion effect within the category, and the competitors' contribution is positive. So it makes sense to set a restriction about non-negative betas.

Also, there is a belief *Awareness' base level* exists. That means, there is a group of people who will recognize the brand, even if they didn't see any ad. In the model this is captured by the Intercept, so we expect it to be positive and lower than *Awareness*.

### Code
The dataset is contained inside the package and is called in the next way:
```{r}
# Load package
library(dlmRStan)

# Load Market Research data
data("dlmRStan3")
dataset <- dlmRStan3
```
We have defined the *Awareness* as the dependent variable, and the intercept's presence has been explained, so the formula should be
```{r}
formula <- awareness ~ .
```
If the intercept's presence didn't make sense, formula would be written 
```{r}
formula <- awareness ~ . + 0
```
Then, the model is fitted, restricting some parameters' values because of the context. Also the MCMC algorithm's parameters are modified for the fitting process to run faster.

```{r}
model <- dlmRStan(
  formula = formula,
  dataset = dataset,
  betas_range = c(0, 0.07),
  intercept_range = c(30,50),
  chains = 4,
  iter = 1000,
  warmup = 500
)

```
Once the model is fitted, a validation of the results is done. This includes
- Mean squared errors (MSE),
- Mean absolute errors (MAE)
- Soft pseudo-squared R (squared correlation between the real data and the model's mean prediction)
- Hard pseudo-squared R (squared correlation between the real data's changes and the model's mean prediction ones, regarding the previous period)
- [loo](https://www.rdocumentation.org/packages/loo/versions/1.0.0/topics/loo)
- [waic](https://www.rdocumentation.org/packages/blmeco/versions/1.1/topics/WAIC)
```{r}s
model$validation
```

Some standard insights are extracted and ploted, including the model's adjustment, the mean betas' values (efficiencies) and the covariates' contribution for each period.
```{r}
model$insights$plots
```
![Image of model_adjustment](https://github.com/opardo/dlmRStan/blob/master/images/model_adjustment.png)
![Image of betas](https://github.com/opardo/dlmRStan/blob/master/images/betas.png)
![Image of contributions](https://github.com/opardo/dlmRStan/blob/master/images/contributions.png)

The data used to create these plots is accessible by writing
```{r}s
model$insights$tables
```

Finally, if an uncertainity's measure is needed, the standard deviation for each parameter estimation can be found in
```{r}s
model$fit$parameters$beta_sd
model$fit$parameters$contribution_sd
```

## Test

TODO
