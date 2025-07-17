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
  real  alpha0; 
  real<lower = 0> alpha0sd;
  real  beta0; 
  real<lower = 0> beta0sd;
  array[nyear] real a;
  array[nyear, nsources] real b;
}


model { 
  // Matrix with site and year specific psi
  matrix[nsite, nyear] psi;
  for(i in 1:nsite) {
    for(t in 1:nyear) {
      psi[i, t] = inv_logit(a[t]);
    }
  }
  
  // Matrix with source and year specific p
  matrix[nyear, nsources] p;
  for(t in 1:nyear) {
    for(s in 1:nsources) {
      p[t, s] = inv_logit(b[t,s]);
    }
  }
  
  // priors
  alpha0 ~ normal(0, 5);
  alpha0sd ~ cauchy(0, 5);
  beta0 ~ normal(0, 5);
  beta0sd ~ cauchy(0, 5);
  
  // random year effect
  for(t in 1:nyear) {
    a[t] ~ normal(alpha0, alpha0sd);
      for(s in 1:nsources) {
        b[t, s] ~ normal(beta0, beta0sd);
    }
  }
  
  // likelihood
  for(o in 1:nobs) {
    if(x[o] == 1) 
      target += log(psi[siteID[o], yearID[o]]) + binomial_lpmf(y[o] | nrep[o], p[program[o]]);
    if(x[o] == 0) {
      target += log_sum_exp(
        log1m(psi[siteID[o], yearID[o]]),
        log(psi[siteID[o], yearID[o]]) + binomial_lpmf(y[o] | nrep[o], p[yearID[o], program[o]]));
    }
  }

}
