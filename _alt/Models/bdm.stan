data {
  int<lower = 0> nobs;
  int<lower = 0> nyear;
  array[nobs] int<lower=0, upper = 1> y;
  array[nobs] int<lower = 0> yearID;
}

parameters {
  real  b0; 
  real<lower = 0> b0sd;
  array[nyear] real a;
}

transformed parameters {
  array[nyear] real psi;
  for (t in 1:nyear) {
    psi[t] = inv_logit(a[t]);
  }
}

model { 
  // priors
  b0 ~ normal(0, 5);
  b0sd ~ cauchy(0, 2);
  
  // random year effect
  for(t in 1:nyear) {
    a[t] ~ normal(b0, b0sd);
  }
  
  // likelihood
  for(o in 1:nobs) {
    y[o] ~ bernoulli(psi[yearID[o]]);
  }

}
