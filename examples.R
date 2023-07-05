library(rstan)
library(shinystan)
options(mc.cores=16)

code <- c('
data {
  int n;
  real y[n];
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  for (i in 1:n)
   y[i] ~ normal(mu,sigma);
  mu ~ normal(1.7,0.3);
  sigma ~ cauchy(0,1);
}')

# Simulating some data
n = 1000
y = rnorm(n,1.6,0.2)

# Running stan code
model = stan_model(model_code=code)

fit = sampling(model,list(n=n,y=y),iter=200,chains=4)

print(fit)

params = extract(fit)

par(mfrow=c(1,2))
ts.plot(params$mu,xlab="Iterations",ylab="mu")
hist(params$sigma,main="",xlab="sigma")
