##
## Example 2: JAGS
##

## Load rjags library
library(rjags)

## Data
k <- 10
x <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y <- c(1, 2, 2, 6, 4, 5, 8, 9, 9, 9, 10)
n <- length(x)

#pdf("example2.pdf", width = 240/72, height = 240/72,
#    family = "Helvetica", pointsize = 10)
plot(x, y, las = 1)
#dev.off()

## Number of chains
n.chains <- 3

## Initial values
inits <- vector("list", n.chains)
inits[[1]] <- list(beta = -10, beta.x = 0,
                   .RNG.seed = 314,
                   .RNG.name = "base::Mersenne-Twister")
inits[[2]] <- list(beta =  -5, beta.x = 2,
                   .RNG.seed = 3141,
                   .RNG.name = "base::Mersenne-Twister")
inits[[3]] <- list(beta =   0, beta.x = 4,
                   .RNG.seed = 31415,
                   .RNG.name = "base::Mersenne-Twister")

## Model file
model.file <- "example2_model.txt"

## Parameters
pars <- c("beta", "beta.x")

## Model
model <- jags.model(file = model.file,
                    data = list(N = n, K = k,
                                X = x, Y = y),
                    inits = inits, n.chains = n.chains,
                    n.adapt = 1000)
## Burn-in
update(model, n.iter = 1000)

## Sampling
post <- coda.samples(model, n.iter = 4000, thin = 4,
                     variable.names = pars)

## Plot results
#pdf("example2_jags_results.pdf", width = 400/72, height = 400/72,
#    family = "Helvetica", pointsize = 10)
plot(post)
#dev.off()

## Check convergence
gelman.diag(post)

## Print results
summary(post)

## Plot expected and observed values
beta <- unlist(post[, "beta"])
beta.x <- unlist(post[, "beta.x"])

new.x <- seq(0, 10, len = 100)
logit.p <- beta + beta.x %o% new.x
exp.y <- k * exp(logit.p) / (1 + exp(logit.p))
y.mean <- apply(exp.y, 2, mean)
y.975 <- apply(exp.y, 2, quantile, probs = 0.975)
y.025 <- apply(exp.y, 2, quantile, probs = 0.025)
y.995 <- apply(exp.y, 2, quantile, probs = 0.995)
y.005 <- apply(exp.y, 2, quantile, probs = 0.005)

#pdf("example2exp.pdf", width = 360/72, height = 360/72,
#    family = "Helvetica", pointsize = 10)
plot(x, y, type = "p", ylim = c(0, 10), las = 1)
lines(new.x, y.mean, lty = 1)
lines(new.x, y.975, lty = 2)
lines(new.x, y.025, lty = 2)
lines(new.x, y.995, lty = 3)
lines(new.x, y.005, lty = 3)
legend("bottomright", legend = c("mean", "95%", "99%"),
       lty = c(1, 2, 3))
#dev.off()
