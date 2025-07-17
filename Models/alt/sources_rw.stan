data {
  int<lower = 0> nobs;
  int<lower = 0> nsite;
  int<lower = 0> nyear;
  array[nobs] int<lower=0> nrep;
  int<lower = 0> nsources;
  array[nobs] int<lower=0> y;
  array[nobs] int<lower = 0, upper = 1> x;
  array[nobs] int<lower = 0> siteID;
  array[nobs] int<lower = 0> yearID;
  array[nobs] int<lower = 1, upper = nsources> program;
}

parameters {
  real  b0; 
  real<lower = 0> b0sd;
  array[nyear] real a;
  array[nsources] real<lower = 0, upper = 1>  p;
}


model { 
  // Matrix with site and year specific psi
  matrix[nsite, nyear] psi;
  for(i in 1:nsite) {
    for(t in 1:nyear) {
      psi[i, t] = inv_logit(a[t]);
    }
  }
  
  // priors
  b0 ~ normal(0, 5);
  b0sd ~ cauchy(0, 5);
  
  // random walk for year effect
  a[1] ~ normal(0, 5);
  for(t in 2:nyear) {
    a[t] ~ normal(a[t-1], b0sd);
  }
  
  // likelihood
  for(o in 1:nobs) {
    if(x[o] == 1) 
      target += log(psi[siteID[o], yearID[o]]) + binomial_lpmf(y[o] | nrep[o], p[program[o]]);
    if(x[o] == 0) {
      target += log_sum_exp(
        log1m(psi[siteID[o], yearID[o]]),
        log(psi[siteID[o], yearID[o]]) + binomial_lpmf(y[o] | nrep[o], p[program[o]]));
    }
  }

}
