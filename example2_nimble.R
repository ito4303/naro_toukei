library(nimble)

## Data
k <- 10
x <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y <- c(1, 2, 2, 6, 4, 5, 8, 9, 9, 9, 10)
n <- length(x)

## Code
ex2.code <- nimbleCode({
  # Likelihood
  for (i in 1:N) {
    logit(p[i]) <- beta + beta.x * X[i]
    Y[i] ~ dbin(p[i], K)
  }
  
  # Priors
  beta ~ dnorm(0, 1.0E-4)
  beta.x ~ dnorm(0, 1.0E-4)
})

ex2.const <- list(K = k, N = n)
ex2.data <- list(X = x, Y = y)
ex2.init <- list(list(beta = -10, beta.x = 0),
                 list(beta =  -5, beta.x = 2),
                 list(beta =   0, beta.x = 4))
ex2.out <- nimbleMCMC(code = ex2.code, constants = ex2.const, data = ex2.data,
                      inits = ex2.init, setSeed = 1,
                      nchains = 3, niter = 6000, nburnin = 1000, thin = 5,
                      samplesAsCodaMCMC = TRUE, summary = TRUE)
ex2.out$summary
summary(ex2.out$samples)
gelman.diag(ex2.out$samples)

plot(ex2.out$samples)
