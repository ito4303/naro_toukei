##
## Example 2: MCMCpack
##

## Load MCMCpack library
library(MCMCpack)

## Data
x <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y <- c(1, 2, 2, 6, 4, 5, 8, 9, 9, 9, 10)

#pdf("example2.pdf", width = 240/72, height = 240/72,
#    family = "Helvetica", pointsize = 10)
plot(x, y, las = 1)
#dev.off()

k <- 10
data <- data.frame(x = rep(x, each = k),
                   y = c(sapply(y, function(i)
                                     c(rep(0, k - i), rep(1, i)))))

## Use MCMClogit function
chains <- 1:3
inits <- c(1, 10, 20)
seeds <- c(12, 123, 1234)
post <- lapply(chains,
               function(chain) {
                 MCMClogit(y ~ x,
                           data = data,
                           burnin = 2000, mcmc = 2000,
                           thin = 2,
                           tune = 1.1, verbose = 500,
                           seed = seeds[chain])
                })
post.mcmc <- mcmc.list(post)

## Plot results
#pdf("example2_results.pdf", width = 400/72, height = 400/72,
#    family = "Helvetica", pointsize = 10)
plot(post.mcmc)
#dev.off()

## Check convergence
gelman.diag(post.mcmc)

## Print results
summary(post.mcmc)

