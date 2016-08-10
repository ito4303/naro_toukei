##
## Example 1: MCMCpack
##

## Data
x <- c(3, 2, 4, 3, 3, 6, 4, 1, 6, 4, 
       5, 7, 4, 4, 1, 4, 0, 3, 8, 4)

## Histogram
h <- hist(x, right = FALSE,
          breaks = seq(min(x), max(x) + 1, 1),
          plot = FALSE)
barplot(h$count, names.arg = h$breaks[-length(h$breaks)],
        las = 1, xlab = "x", ylab = "count")

## Mean and variance
mean(x)
var(x)

## GLM
fit <- glm(x ~ 1, family = poisson(link = log))
summary(fit)
print(exp(coef(fit)))


## Load MCMCpack library
library(MCMCpack)

## Use MCMCpoisson function
post1 <- vector("list", 3)
post1[[1]] <- MCMCpoisson(x ~ 1, beta.start = 1,
                          burnin = 0, mcmc = 400,
                          thin = 1,
                          tune = 0.8, seed = 1117,
                          verbose = 20)

## Plot trace
#pdf("example1-1.pdf", width = 360/72, height = 240/72,
#    family = "Helvetica", pointsize = 10)
plot(post1[[1]], density = FALSE, col = 1, las = 1)
#dev.off()

post1[[2]] <- MCMCpoisson(x ~ 1, beta.start = 5,
                          burnin = 0, mcmc = 400,
                          thin = 1,
                          tune = 0.8, seed = 1123,
                          verbose = 20)
post1[[3]] <- MCMCpoisson(x ~ 1, beta.start = 10,
                          burnin = 0, mcmc = 400,
                          thin = 1,
                          tune = 0.8, seed = 1129,
                          verbose = 20)

plot(post1[[2]], density = FALSE, col = 2, las = 1)
plot(post1[[3]], density = FALSE, col = 3, las = 1)

## Tranform into mcmc.list class
post1.mcmc <- mcmc.list(post1)

## Plot trace
#pdf("example1-2.pdf", width = 360/72, height = 240/72,
#    family = "Helvetica", pointsize = 10)
plot(post1.mcmc, density = FALSE,
     ylim = c(0, 10), las = 1)
#dev.off()


## Adjust burnin, mcmc and tune parameters
post2 <- vector("list", 3)
burnin <- 500
mcmc <- 1000
thin <- 1
tune <- 2
verbose <- 0
post2[[1]] <- MCMCpoisson(x ~ 1, beta.start = 1,
                          burnin = burnin, mcmc = mcmc,
                          thin = thin,
                          tune = tune, seed = 1117,
                          verbose = verbose)
post2[[2]] <- MCMCpoisson(x ~ 1, beta.start = 5,
                          burnin = burnin, mcmc = mcmc,
                          thin = thin,
                          tune = tune, seed = 1123,
                          verbose = verbose)
post2[[3]] <- MCMCpoisson(x ~ 1, beta.start = 10,
                          burnin = burnin, mcmc = mcmc,
                          thin = thin,
                          tune = tune, seed = 1129,
                          verbose = verbose)
post2.mcmc <- mcmc.list(post2)

#pdf("example1-3.pdf", width = 360/72, height = 240/72,
#    family = "Helvetica", pointsize = 10)
plot(post2.mcmc)
#dev.off()

## Summary
summary(post2.mcmc)

## Exponential of poseterior mean of lambda
exp(summary(post2.mcmc)$statistics["Mean"])

## Convergence diagnostic
# Gelman & Rubin
gelman.diag(post2.mcmc)

# Geweke
geweke.diag(post2.mcmc)


##
## Use MCMCmetrop1R function
##

## Function returns log of prior x likelihood
LogPoisFun <- function(lambda, x) {
  # Poisson distribution: p(x) = lambda^x exp(-lambda)/x!
  if (lambda >= 0) {    # lambda must be non-negative
    # prior: dunif(0, 10^4)
    log(ifelse(lambda >= 0 & lambda < 10^4, 10^-4, 0)) + 
      # log likelihood
      sum(log(lambda^x * exp(-lambda) / factorial(x)))
  } else {
    -Inf
  }
}

## MCMC using Metropolis method
# burn-in: 1000
# iteration: 1000
# thinning: 1
chains <- 1:3
inits <- c(1, 10, 20)
seeds <- c(1117, 1123, 1129)
post3 <- lapply(chains,
                function(chain) {
                  MCMCmetrop1R(fun = LogPoisFun,
                               theta.init = inits[chain],
                               burnin = 1000, mcmc = 1000,
                               thin = 1,
                               tune = 2, seed = seeds[chain],
                               verbose = 0, logfun = TRUE,
                               x = x)
                })
post3.mcmc <- mcmc.list(post3)

## Convergence diagnostic
# Gelman & Rubin
gelman.diag(post3.mcmc)

## Plot trace
plot(post3.mcmc)

## Show results
summary(post3.mcmc)

## Informative prior

## Function for sampling
LogPoisFun2 <- function(lambda, x) {
  if (lambda >= 0) {  # lambda must be non-negative
    # prior: Gamma(2, 2)
    log(dgamma(lambda, shape = 2, rate = 2)) + 
      # log likelihood
      sum(log(dpois(x, lambda)))
  } else {
    -Inf
  }
}

chains <- 1:3
inits <- c(1, 10, 20)
seeds <- c(1117, 1123, 1129)
post4 <- lapply(chains,
                function(i) {
                  MCMCmetrop1R(fun = LogPoisFun2,
                               theta.init = inits[i],
                               burnin = 2000, mcmc = 2000,
                               thin = 1,
                               tune = 2, seed = seeds[i],
                               verbose = 0, logfun = TRUE,
                               x = x)
                })
post4.mcmc <- mcmc.list(post4)
summary(post4.mcmc)
plot(post4.mcmc, trace = TRUE, density = TRUE, las = 1)

## Plot prior, posterior and likelihood
#pdf("example1-4.pdf", width = 480/72, height = 480/72,
#    family = "Helvetica", pointsize = 10)

par(mfrow = c(2, 1))
# Prior
curve(dgamma(x, shape = 2, rate= 2), from = 0, to = 5,
      lty = 2, las = 1,
      xlim = c(0, 5), ylim = c(0, 1.2),
      xlab = "lambda", ylab = "density")

# Posterior
lines(density(sapply(1:3, function(i) post4[[i]])))

# Likelihood
ll <- function(x, x0) {
  sapply(x, function(lambda) prod(dpois(x0, lambda)))
}

x0 <- x
curve(ll(x, x0 = x0), from = 0, to = 5,
      type = "l", las = 1,
      xlab = "lambda", ylab = "likelihood")
#dev.off()


