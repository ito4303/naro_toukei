data {
  int<lower = 0> N;
  int<lower = 0> K;
  vector[N] X;
  int<lower = 0, upper = K> Y[N];
}

parameters {
  real beta;
  real beta_x;
}

transformed parameters {
  vector[N] logit_p = beta + beta_x * X;
}

model {
  // Likelihood
  Y ~ binomial_logit(K, logit_p);

  // Priors
  beta ~ normal(0.0, 1.0e+2);
  beta_x ~ normal(0.0, 1.0e+2);
}
