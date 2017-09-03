##
## Example 1: Poisson regression
##

## Data
x <- c(3, 2, 4, 3, 3, 6, 4, 1, 6, 4,
       5, 7, 4, 4, 1, 4, 0, 3, 8, 4)

## Histogram
h <- hist(x, right = FALSE,
          breaks = seq(min(x), max(x) + 1, 1),
          plot = FALSE)
barplot(h$counts, names.arg = h$breaks[-length(h$breaks)],
        las = 1, xlab = "x", ylab = "count")

## Mean and variance
mean(x)
var(x)

## GLM
fit <- glm(x ~ 1, family = poisson(link = log))
summary(fit)
print(exp(coef(fit)))

## Load rjags library
library(rjags)

## Run JAGS
inits <- vector("list", 3)
inits[[1]] <- list(lambda = 1,
                   .RNG.seed = 1,
                   .RNG.name = "base::Mersenne-Twister")
model1 <- jags.model("example1_model.txt",
                     data = list(X = x, N = length(x)),
                     inits = inits[[1]], n.chains = 1, n.adapt = 0)
post1 <- coda.samples(model1, variable.names = "lambda",
                      n.iter = 50)

## Plot trace
#pdf("example1-1.pdf", width = 360/72, height = 240/72,
#    family = "Helvetica", pointsize = 10)
traceplot(post1, col = 1, las = 1)
#dev.off()

inits[[2]] <- list(lambda = 30,
                   .RNG.seed = 2,
                   .RNG.name = "base::Mersenne-Twister")
inits[[3]] <- list(lambda = 100,
                   .RNG.seed = 3,
                   .RNG.name = "base::Mersenne-Twister")
model2 <- jags.model("example1_model.txt",
                     data = list(X = x, N = length(x)),
                     inits = inits, n.chains = 3, n.adapt = 0)
post2 <- coda.samples(model2, variable.names = "lambda",
                      n.iter = 50)

## Plot trace
#pdf("example1-2.pdf", width = 360/72, height = 240/72,
#    family = "Helvetica", pointsize = 10)
traceplot(post2, las = 1, col = c(1, 2, 4))
#dev.off()


## Adjust burnin, mcmc and tune parameters
burnin <- 1000
iter <- 1000
thin <- 1
model3 <- jags.model("example1_model.txt",
                     data = list(X = x, N = length(x)),
                     inits = inits, n.chains = 3, n.adapt = 500)
update(model3, n.iter = burnin - 500)
post3 <- coda.samples(model3,
                      variable.names = "lambda",
                      n.iter = iter, thin = thin)

#pdf("example1-3.pdf", width = 360/72, height = 240/72,
#    family = "Helvetica", pointsize = 10)
plot(post3)
#dev.off()

## Summary
summary(post3)

## Convergence diagnostic
# Gelman & Rubin
gelman.diag(post3)


## Informatvie prior
inits <- list(list(lambda = 0.1,
                   .RNG.seed = 1,
                   .RNG.name = "base::Mersenne-Twister"),
              list(lambda = 1,
                   .RNG.seed = 2,
                   .RNG.name = "base::Mersenne-Twister"),
              list(lambda = 10,
                   .RNG.seed = 3,
                   .RNG.name = "base::Mersenne-Twister"))
model4 <- jags.model("example1-1_model.txt",
                     data = list(X = x, N = length(x)),
                     inits = inits, n.chains = 3, n.adapt = 500)
update(model4, n.iter = burnin - 500)
post4 <- coda.samples(model4,
                      variable.names = "lambda",
                      n.iter = iter, thin = thin)
summary(post4)

## Plot prior, posterior and likelihood
#pdf("example1-4.pdf", width = 480/72, height = 480/72,
#    family = "Helvetica", pointsize = 10)

par(mfrow = c(2, 1))
# Prior
curve(dgamma(x, shape = 2, rate = 2), from = 0, to = 5,
      lty = 2, las = 1,
      xlim = c(0, 5), ylim = c(0, 1.2),
      xlab = "lambda", ylab = "density")

# Posterior
lines(density(sapply(1:3, function(i) post4[[i]])))

# Likelihood
ll <- function(x, x0) {
  sapply(x, function(lambda) prod(dpois(x0, lambda)))
}

x0 <- x  # Preserve x as x0
curve(ll(x, x0 = x0), from = 0, to = 5,
      type = "l", las = 1,
      xlab = "lambda", ylab = "likelihood")
#dev.off()
par(mfrow = c(1, 1))
