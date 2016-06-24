//
// Zero-inflated Poisson model
//

data {
  int<lower=0> N;          // Number of observations
  int<lower=0> Y[N];       // Number of new seedlings
  real<lower=0> X[N];      // Proportion of open canopy
}
parameters {
  real<lower=0,upper=1> p; // Probability of the presence
  real beta[2];            // Intercept and coefficient
}
transformed parameters {
  real log_lambda[N];      // Log of Poisson mean

  for (i in 1:N) {
    log_lambda[i] <- beta[1] + beta[2] * X[i];
  }
}
model {
  // Improper uniform priors are implicitly defined on p and beta.
  for (i in 1:N) {
    if (Y[i] > 0) {
      // Bernoulli(1|p) * Poisson(Y|λ)
      increment_log_prob(bernoulli_log(1, p) + poisson_log_log(Y[i], log_lambda[i])); 
    } else { // Y[i] == 0
      // Bernoulli(0|p) + Bernoulli(1|p) * Poisson(0|λ)
      increment_log_prob(log_sum_exp(bernoulli_log(0, p),
                                     bernoulli_log(1, p)
                                     + poisson_log_log(0, log_lambda[i])));
    }
  }
}
