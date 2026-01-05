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
  real  c0; 
  real<lower = 0> c0sd;
  array[nyear] real a;
  array[nyear, nsources] real<lower = 0, upper = 1>  b;
}

transformed parameters {
  array[nyear] real psi;
  array[nyear, nsources] real p;
  for (t in 1:nyear) {
    psi[t] = inv_logit(a[t]);
    for(s in 1:nsources) {
      p[t,s] = inv_logit(b[t,s]);
    }
  }
}

model { 
  
  // priors
  b0 ~ normal(0, 5);
  b0sd ~ cauchy(0, 2);
  c0 ~ normal(0, 5);
  c0sd ~ cauchy(0, 2);
  
  // random effects
  for(t in 1:nyear) {
    a[t] ~ normal(b0, b0sd);
    for(s in 1:nsources) {
      b[t,s] ~ normal(c0, c0sd);
    }
  }
  
  // likelihood
  for(o in 1:nobs) {
    if(x[o] == 1) 
      target += log(psi[yearID[o]]) + binomial_lpmf(y[o] | nrep[o], p[yearID[o], program[o]]);
    if(x[o] == 0) {
      target += log_sum_exp(
        log1m(psi[yearID[o]]),
        log(psi[yearID[o]]) + binomial_lpmf(y[o] | nrep[o], p[yearID[o], program[o]]));
    }
  }

}
