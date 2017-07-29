data {
  int<lower = 0> N;            // Number of data records
  int<lower = 0> M;            // Number of blocks
  int<lower = 1> L;            // Number of coefficients
  row_vector[L] X[N];          // Data
  real Y[N];
  int<lower = 1, upper = M> B[N]; // Block
}

parameters {
  real e[M];                   // Random effect
  vector[L] beta;
  real<lower = 0> sigma[2];
}

transformed parameters {
  vector[N] mu;                // Local variable

  // Mean values
  for (i in 1:N)
    mu[i] = X[i] * beta + e[B[i]];
}

model {
  // Improper uniform priors are implicitly defined on e, beta,
  // and sigma.

  // Vectorized
  Y ~ normal(mu, sigma[1]);
  e ~ normal(0, sigma[2]);
}
