##
## Example 3: Linear Mixed Model
##

library(rjags)
load.module("glm")

## Data
data <- read.csv("example3.csv")

## Show data
head(data, 10)

#pdf("example3_pairs.pdf", width = 360/72, height = 360/72)

pairs(data)
#dev.off()

## Convert data to matrices
n.block <- max(data$block)      # number of blocks
n.data <- nrow(data) / n.block  # number of observations per block

x1 <- t(matrix(data$x1, nrow = n.data, ncol = n.block))
x2 <- t(matrix(data$x2, nrow = n.data, ncol = n.block))
y  <- t(matrix(data$y,  nrow = n.data, ncol = n.block))

print(x1)

## Model file
model.file <- "example3_model.txt"

## Number of chains
n.chains <- 3

## Initial values
inits <- vector("list", n.chains)
inits[[1]] <- list(beta =  5, beta.1 = 0, beta.2 = 0,
                   sigma = 1, sigma.B = 1,
                   .RNG.seed = 123,
                   .RNG.name = "base::Mersenne-Twister")
inits[[2]] <- list(beta =  -5, beta.1 = 10,  beta.2 = 10,
                   sigma = 10, sigma.B = 10,
                   .RNG.seed = 1234,
                   .RNG.name = "base::Mersenne-Twister")
inits[[3]] <- list(beta = 0, beta.1 = -10,  beta.2 = -10,
                   sigma = 5, sigma.B = 5,
                   .RNG.seed = 12345,
                   .RNG.name = "base::Mersenne-Twister")

## Parameters
pars <- c("beta", "beta.1", "beta.2",
          "sigma", "sigma.B", "e.B")

## MCMC
model <- jags.model(file = model.file,
                    data = list(M = n.block, N = n.data,
                                X1 = x1, X2 = x2, Y = y),
                    inits = inits, n.chains = n.chains,
                    n.adapt = 1000)

## Burn-in
update(model, n.iter = 1000)

## Sampling
post <- coda.samples(model, n.iter = 5000, thin = 5,
                     variable.names = pars)

## Show results
gelman.diag(post)
summary(post)

## Plot densities of random effects
#pdf("example3_e.pdf", width = 360/72, height = 360/72,
#    family = "Helvetica", pointsize = 10)
plot(NULL, type = "n",
     xlim = c(-4, 4), ylim = c(0, 0.8),
     xlab = "value", ylab = "density",
     main = "Posterior distribution of e.B[]",
     las = 1)
for (i in 1:n.block) {
  j <- paste("e.B[", i, "]", sep = "")
  lines(density(unlist(post[, j])), col = i)
}
#dev.off()


##
## Nested indexing
##

## Data
data <- read.csv("example3.csv")
n.block <- max(data$block)      # Number of blocks
n.data <- nrow(data)            # Number of observations

## Model
model.file <- "example3-1_model.txt"

# Number of chains
n.chains <- 3

## Initial values
inits <- vector("list", n.chains)
inits[[1]] <- list(beta =  5, beta.1 = 0, beta.2 = 0,
                   sigma = 1, sigma.B = 1,
                   .RNG.seed = 123,
                   .RNG.name = "base::Mersenne-Twister")
inits[[2]] <- list(beta =  -5, beta.1 = 10,  beta.2 = 10,
                   sigma = 10, sigma.B = 10,
                   .RNG.seed = 1234,
                   .RNG.name = "base::Mersenne-Twister")
inits[[3]] <- list(beta = 0, beta.1 = -10,  beta.2 = -10,
                   sigma = 5, sigma.B = 5,
                   .RNG.seed = 12345,
                   .RNG.name = "base::Mersenne-Twister")

## Parameters
pars <- c("beta", "beta.1", "beta.2",
          "sigma", "sigma.B", "e.B")

## MCMC
model <- jags.model(file = model.file,
                    data = list(M = n.block, N = n.data,
                                X1 = data$x1, X2 = data$x2,
                                Y = data$y, B = data$block),
                    inits = inits, n.chains = n.chains,
                    n.adapt = 1000)
## Burn-in
update(model, n.iter = 1000)

## Sampling
post <- coda.samples(model, n.iter = 5000, thin = 5,
                     variable.names = pars)

## Show results
gelman.diag(post)
summary(post)

## Visualization using ggmcmc
library(ggmcmc)

post.ggs <- ggs(post)
ggs_caterpillar(post.ggs, "e.B")
#ggsave("example3_caterpillar.pdf", width = 12, height = 12, units = "cm")

ggmcmc(post.ggs, "example3-output.pdf")
