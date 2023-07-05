library(rstan)
library(shinystan)

options(mc.cores=16)

model.code <- c('data {
  int<lower=0> N;
  int<lower=0> y[N];
}
parameters {
  real<lower=0, upper=1> theta;
  real<lower=0> lambda;
  real<lower=0> scale;
}
model {
  scale ~ cauchy(0,25);
  lambda ~ gamma(2, 1/scale);
  theta ~ beta(1, 1);
  
  for (n in 1:N) {
    if (y[n] == 0)
      target += log_sum_exp(bernoulli_lpmf(1 | theta),
                            bernoulli_lpmf(0 | theta)
                              + poisson_lpmf(y[n] | lambda));
    else
      target += bernoulli_lpmf(0 | theta)
                  + poisson_lpmf(y[n] | lambda);
  }
  
}')

theta <- 0.25
lambda <- 10

n <- 100
y <- rbinom(n, 1, 1-theta)*rpois(n, lambda)

model = stan_model(model_code=model.code)

fit = sampling(model,list(N=n,y=y),iter=2000,chains=16)

launch_shinystan(fit, host="0.0.0.0", port=4242)
