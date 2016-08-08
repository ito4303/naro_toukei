##
## Example 4 using Stan
## Zero-Inflated Poisson Model
##

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## Data
data <- read.csv("example4.csv")

## Run Stan
fit <- stan("example4.stan",
            data = list(N = nrow(data),
                        Y = data$Num,
                        X = data$Light),
            pars = c("p", "beta"),
            chains = 4, iter = 6000, warmup = 1000, thin = 5)

plot(fit)
traceplot(fit)
print(fit, digits = 3)
