##
## Example 2: Stan
##

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# data
k <- 10
x <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y <- c(1, 2, 2, 6, 4, 5, 8, 9, 9, 9, 10)
n <- length(x)

# pdf("example2.pdf", width = 240/72, height = 240/72,
#    family = "Helvetica", pointsize = 10)
# plot(x, y, las = 1)
# dev.off()

# model
example2_code <- "
"

# number of chains
n.chains <- 3

# initial values
inits <- vector("list", 3)
inits[[1]] <- list(beta = -10, beta_x = 0)
inits[[2]] <- list(beta =  -5, beta_x = 2)
inits[[3]] <- list(beta =   0, beta_x = -2)

# parameters
pars <- c("beta", "beta_x")

# run Stan
fit <- stan("example2.stan",
            data = list(X = x, Y = y, N = n, K = k),
            pars = pars, init = inits, seed = 123,
            chains = n.chains,
            iter = 2500, warmup = 500, thin = 2)


# plot trace
rstan::traceplot(fit)

# show results
print(fit)
