##
## Example 4: Zero-Inflated Poisson Model
##

library(rjags)
load.module("glm")

## Data
data <- read.csv("example4.csv")

## Plot data
#pdf("example4_scatter.pdf", width = 360/72, height = 270/72,
#    family = "Helvetica", pointsize = 10)
plot(Num ~ Light, data = data, las = 1, cex = 1.5)
#dev.off()

## GLM
summary(glm(Num ~ Light, family = poisson, data = data))

## Number of 0
h <- hist(data$Num, breaks = 0:(max(data$Num) + 1),
          right = FALSE, plot = FALSE)
#pdf("example4_barplot.pdf", width = 360/72, height = 270/72,
#    family = "Helvetica", pointsize = 10)
barplot(h$count, names = h$breaks[1:(max(data$Num) + 1)],
        xlab = "Number of seedlings", ylab = "Frequency",
        ylim = c(0, 25), las = 1)
#dev.off()

## Model
model.file <- "example4_model.txt"

## Number of chains
n.chains <- 3

## Initial values
inits <- vector("list", n.chains)
inits[[1]] <- list(p =  0.1, beta = 0, beta.x = 1,
                   z = as.numeric(data$Num > 0),
                   .RNG.seed = 123,
                   .RNG.name = "base::Mersenne-Twister")
inits[[2]] <- list(p =  0.5, beta = -1,  beta.x = -1,
                   z = as.numeric(data$Num > 0),
                   .RNG.seed = 1234,
                   .RNG.name = "base::Mersenne-Twister")
inits[[3]] <- list(p = 0.9, beta = 1,  beta.x = 0,
                   z = as.numeric(data$Num > 0),
                   .RNG.seed = 12345,
                   .RNG.name = "base::Mersenne-Twister")

## Parameters
pars <- c("p", "beta", "beta.x")

## MCMC
model <- jags.model(file = model.file,
                    data = list(N = nrow(data),
                                Y = data$Num,
                                X = data$Light),
                    inits = inits, n.chains = n.chains,
                    n.adapt = 1000)

## Burn-in
update(model, n.iter = 1000)

## Sampling
post <- coda.samples(model, n.iter = 10000, thin = 10,
                     variable.names = pars)

## Show results
gelman.diag(post)
summary(post)

samp.beta.x <- unlist(post[, "beta.x"])
sum(samp.beta.x > 0) / length(samp.beta.x)
