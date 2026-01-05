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
  real<lower = 0> b0sd;
  array[nyear] real a;
  array[nsources] real c0;
  array[nsources] real c1;
}

transformed parameters {
  array[nyear] real psi;
  array[nyear, nsources] real p;
  for (t in 1:nyear) {
    psi[t] = inv_logit(a[t]);
    p[t,1] = inv_logit(c0[1]);
    for(s in 2:nsources) {
      p[t,s] = inv_logit(c0[s] + c1[s] * t);
    }
  }
}

model { 
  
  // priors
  b0sd ~ normal(0, 1);
  a[1] ~ normal(0, 5);
  for(s in 1: nsources) {
      c0[s] ~ normal(0, 5);
      c1[s] ~ normal(0, 5);
  }  
  // Random walk
  for(t in 2:nyear) {
    a[t] ~ normal(a[t-1], b0sd);
  }
  
  // likelihood
  for(o in 1:nobs) {
    if(x[o] == 1) 
      target += log(psi[yearID[o]]) + binomial_lpmf(y[o] | nrep[o], p[yearID[o],program[o]]);
    if(x[o] == 0) {
      target += log_sum_exp(
        log1m(psi[yearID[o]]),
        log(psi[yearID[o]]) + binomial_lpmf(y[o] | nrep[o], p[yearID[o],program[o]]));
    }
  }

}
