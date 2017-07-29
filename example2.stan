data {
  int<lower = 0> N;
  int<lower = 0> K;
  vector[N] X;
  int<lower = 0, upper = 10> Y[N];
}

parameters {
  real beta;
  real beta_x;
}

transformed parameters {
  vector[N] logit_p;

  logit_p = beta + beta_x * X;
}

model {
  // Priors
  beta ~ normal(0.0, 1.0e+3);
  beta_x ~ normal(0.0, 1.0e+3);

  // Likelihood
  Y ~ binomial_logit(K, logit_p);
}
