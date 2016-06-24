##
## Example 3: Stan
##

library(rstan)

# data
data <- read.csv("example3.csv")
n.block <- max(data$block)      # number of blocks
n.data <- nrow(data)            # number of data

model <- stan_model("example3.stan")
fit <- sampling(model,
                data = list(X = cbind(rep(1, n.data), data$x1, data$x2),
                            Y = data$y, B = data$block,
                            M = n.block, N = n.data, L = 3),
                pars = c("beta", "e", "sigma", "sigma_B"),
                chains = 4, iter = 2000, warmup = 1000)
#
# or
#
# fit <- stan("example3.stan",
#             data = list(X = cbind(rep(1, n.data), data$x1, data$x2),
#                         Y = data$y, B = data$block,
#                         M = n.block, N = n.data, L = 3),
#             chains = 4, iter = 2000, warmup = 1000)

#
plot(fit)
traceplot(fit, c("beta[1]", "beta[2]", "beta[3]",
                  "sigma", "sigma_B"))
print(fit, digits = 3)
