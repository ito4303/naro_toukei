//
// Zero-inflated Poisson model
//

data {
  int<lower = 0> N;             // Number of sites
  int<lower = 0> Y[N];          // Number of new seedlings
  vector<lower = 0>[N] X;       // Explanatory variable
}

parameters {
  real<lower = 0, upper = 1> p; // Probability of presence
  real beta[2];                 // Intercept and coefficient
}

transformed parameters {
  // Log of Poisson mean
  vector[N] log_lambda = beta[1] + beta[2] * X;
}

model {
  // Improper uniform priors are implicitly defined on p and beta.
  for (i in 1:N) {
    if (Y[i] > 0) {
      // Bernoulli(1|p) * Poisson(Y|λ)
      1 ~ bernoulli(p);
      Y[i] ~ poisson_log(log_lambda[i]);
    } else {
      // if Y[i] == 0
      // Bernoulli(0|p) + Bernoulli(1|p) * Poisson(0|λ)
      target += log_sum_exp(bernoulli_lpmf(0 | p),
                            bernoulli_lpmf(1 | p)
                          + poisson_log_lpmf(0 | log_lambda[i]));
    }
  }
}
