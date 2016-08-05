//
// Zero-inflated Poisson model
//

data {
  int<lower=0> N;          // Number of observations
  int<lower=0> Y[N];       // Number of new seedlings
  vector<lower=0>[N] X;    // Proportion of open canopy
}
parameters {
  real<lower=0,upper=1> p; // Probability of presence
  real beta[2];            // Intercept and coefficient
}
transformed parameters {
  vector[N] log_lambda;    // Log of Poisson mean

  log_lambda = beta[1] + beta[2] * X;
}
model {
  // Improper uniform priors are implicitly defined on p and beta.
  for (i in 1:N) {
    target += (Y[i] > 0) ?
      // if Y[i] > 0
      // Bernoulli(1|p) * Poisson(Y|λ)
      bernoulli_lpmf(1 | p)
      + poisson_log_lpmf(Y[i] | log_lambda[i]) :
      // if Y[i] == 0
      // Bernoulli(0|p) + Bernoulli(1|p) * Poisson(0|λ)
      log_sum_exp(bernoulli_lpmf(0 | p),
                  bernoulli_lpmf(1 | p)
                  + poisson_log_lpmf(0 | log_lambda[i]));
  }
}
