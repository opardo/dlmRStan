# dlmRStan

## Author

Carlos Omar Pardo (omarpardog@gmail.com)

## Overview

Bayesian linear model in a time series context, with a different beta for each time period, and considering a correlation structure between all of them. This kind of models are also regularly known as dynamic linear models (dlm). 

This package uses [RStan](http://mc-stan.org/users/interfaces/rstan) to develop and run the MCMC algorithm.

## Model

Be one dependent random variable _y<sub>t</sub>_, and a vector of independent variables _x<sub>t</sub>_, so _P(y<sub>t</sub>|x<sub>t</sub>)_ makes sense. Is important to remark that the subindex _t_ is not exchangable, that is to say, the subindex represents the order in time. Then, this package's model can be expressed as

<a href="https://www.codecogs.com/eqnedit.php?latex=\dpi{150}&space;\begin{aligned}&space;y_t|x_t,&space;\beta_t,\sigma^2&space;&\sim&space;\mathcal{N}(x_t^T\beta_t,&space;\sigma^2)&space;\\&space;\sigma^2&space;&\sim&space;Gamma(\sigma_\gamma,&space;\sigma_\delta)&space;\\&space;\beta_t&space;|&space;\beta_{t-1},&space;AF&space;&\sim&space;\mathcal{N}(\beta_{t-1},&space;AF&space;*&space;B),&space;t&space;\geq&space;2\\&space;\beta_1&space;|&space;\mu_1,&space;AF&space;&\sim&space;\mathcal{N}(\mu_1,&space;AF&space;*&space;B)&space;\\&space;\mu_1&space;&\sim&space;\mathcal{N}(b,B)&space;\\&space;AF&space;&\sim&space;Beta(AF_\gamma,AF_\delta).&space;\end{aligned}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\dpi{150}&space;\begin{aligned}&space;y_t|x_t,&space;\beta_t,\sigma^2&space;&\sim&space;\mathcal{N}(x_t^T\beta_t,&space;\sigma^2)&space;\\&space;\sigma^2&space;&\sim&space;Gamma(\sigma_\gamma,&space;\sigma_\delta)&space;\\&space;\beta_t&space;|&space;\beta_{t-1},&space;AF&space;&\sim&space;\mathcal{N}(\beta_{t-1},&space;AF&space;*&space;B),&space;t&space;\geq&space;2\\&space;\beta_1&space;|&space;\mu_1,&space;AF&space;&\sim&space;\mathcal{N}(\mu_1,&space;AF&space;*&space;B)&space;\\&space;\mu_1&space;&\sim&space;\mathcal{N}(b,B)&space;\\&space;AF&space;&\sim&space;Beta(AF_\gamma,AF_\delta).&space;\end{aligned}" title="\begin{aligned} y_t|x_t, \beta_t,\sigma^2 &\sim \mathcal{N}(x_t^T\beta_t, \sigma^2) \\ \sigma^2 &\sim Gamma(\sigma_\gamma, \sigma_\delta) \\ \beta_t | \beta_{t-1}, AF &\sim \mathcal{N}(\beta_{t-1}, AF * B), t \geq 2\\ \beta_1 | \mu_1, AF &\sim \mathcal{N}(\mu_1, AF * B) \\ \mu_1 &\sim \mathcal{N}(b,B) \\ AF &\sim Beta(AF_\gamma,AF_\delta). \end{aligned}" /></a>

This package introduces a novelty with regard to other dlm packages: it allows the modeler to constraint the beta and squared-sigma values with the following restrictions:

<a href="https://www.codecogs.com/eqnedit.php?latex=\dpi{150}&space;\begin{aligned}&space;\beta_{lb}&space;\leq&space;&\beta_t&space;\leq&space;\beta_{ub}&space;\\&space;\sigma_{lb}&space;&\leq&space;\sigma^2.&space;\end{aligned}" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\dpi{150}&space;\begin{aligned}&space;\beta_{lb}&space;\leq&space;&\beta_t&space;\leq&space;\beta_{ub}&space;\\&space;\sigma_{lb}&space;&\leq&space;\sigma^2.&space;\end{aligned}" title="\begin{aligned} \beta_{lb} \leq &\beta_t \leq \beta_{ub} \\ \sigma_{lb} &\leq \sigma^2. \end{aligned}" /></a>

## Installation

First of all, you must [install RTools](http://thecoatlessprofessor.com/programming/rcpp/install-rtools-for-rcpp/), since the algorithm runs in C++.

Then you should run the following code to install the package from Github.

```{r}
install.packages("devtools")
library(devtools)
install_github("opardo/dlmRStan")
```

## Example

### Context
The next example takes place in the market research context, trying to explain [corporate brand](https://en.wikipedia.org/wiki/Brand_architecture)'s *Awareness* with the TV investment data. *Awareness* is a KPI defined as the percentage of people who declares to know the corporate brand. The TV investment is in *[Adstocked](https://en.wikipedia.org/wiki/Advertising_adstock) [GRPs](https://es.wikipedia.org/wiki/Gross_Rating_Points)*, an unit which removes the currency fluctuations, and takes into account how many people and how frequently were exposed to the ad. Also assumes the ad is reminded for some time, even if people don't see it again.

For this specific case, 4 covariates are used:
- Main Thrust: commercials about corporate brand's image
- Sub-brands: advertising for specific products
- Competitor 1
- Competitor 2

*Awareness* is a special metric because the competitors' effect is almost always non-negative. Worst case scenario, customers don't associate the competitors' ads with the studied brand and the contribution to *Awareness* is 0. But in other cases, there is a confussion effect within the category, and the competitors' contribution is positive. So it makes sense to set a restriction about non-negative betas.

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
Once the model is fitted, a validation is done. This includes
- Mean squared errors (MSE),
- Mean absolute errors (MAE)
- Soft pseudo-squared R (squared correlation between the real data and the model's mean prediction)
- Hard pseudo-squared R (squared correlation between the real data's deltas and the model's ones, regarding the previous period)
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
