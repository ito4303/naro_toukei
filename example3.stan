data {
  int<lower=0> N;            // Number of data
  int<lower=0> M;            // Number of blocks
  int<lower=1> L;            // Number of coefficients
  row_vector[L] X[N];        // Data
  real Y[N];
  int<lower=1,upper=M> B[N]; // Block
}
parameters {
  real e[M];                 // Random effect
  vector[L] beta;
  real<lower=0> sigma;
  real<lower=0> sigma_B;
}
model {
  vector[N] mu;              // Local variable

  // Improper uniform priors are implicitly defined on e, beta,
  // sigma and sigma_B.
  
  // Mean values
  for (i in 1:N)
    mu[i] = X[i] * beta + e[B[i]];

  // Vectorized
  Y ~ normal(mu, sigma);
  e ~ normal(0, sigma_B);
}
