data {
  int<lower=0> N;            // number of data
  int<lower=0> M;            // number of blocks
  int<lower=1> L;            // number of coefficients
  row_vector[L] X[N];        // data
  real Y[N];
  int<lower=1,upper=M> B[N]; // block
}
parameters {
  real e[M];                 // random effect
  vector[L] beta;
  real<lower=0> sigma;
  real<lower=0> sigma_B;
}
model {
  // Improper uniform priors are implicitly defined on e, beta, sigma and sigma_b.
  for (i in 1:N) {
    Y[i] ~ normal(X[i] * beta + e[B[i]], sigma);
  }
}
