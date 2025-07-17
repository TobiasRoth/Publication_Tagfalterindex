data {
  int<lower = 0> nobs;
  int<lower = 0> nyear;
  array[nobs] int<lower=0> nrep;
  int<lower = 0> nsources;
  array[nobs] int<lower=0> y;
  array[nobs] int<lower = 0, upper = 1> x;
  array[nobs] int<lower = 0> yearID;
  array[nobs] int<lower = 1, upper = nsources> program;
}

parameters {
  real  b0; 
  real<lower = 0> b0sd;
  array[nyear] real a;
  array[nsources] real<lower = 0, upper = 1>  p;
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
  b0sd ~ cauchy(0, 5);
  
  // random year effect
  for(t in 1:nyear) {
    a[t] ~ normal(b0, b0sd);
  }
  
  // likelihood
  for(o in 1:nobs) {
    if(x[o] == 1) 
      target += log(psi[yearID[o]]) + binomial_lpmf(y[o] | nrep[o], p[program[o]]);
    if(x[o] == 0) {
      target += log_sum_exp(
        log1m(psi[yearID[o]]),
        log(psi[yearID[o]]) + binomial_lpmf(y[o] | nrep[o], p[program[o]]));
    }
  }

}
