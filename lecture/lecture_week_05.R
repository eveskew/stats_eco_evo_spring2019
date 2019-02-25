

library(rethinking)

#==========================================================


# Plot probability mass function for
# binomial trial of size = 10, prob = 0.3
plot(0:10, dbinom(0:10, size = 10, prob = 0.3))

# The probability of a single outcome
dbinom(5, size = 10, prob = 0.3)

# The probability mass of all outcomes sums to 1
sum(dbinom(0:10, size = 10, prob = 0.3))

# 10 simulated draws from the binomial distribution
draws <- rbinom(10, size = 10, prob = 0.3)
simplehist(draws, xlim = c(0, 10))

# 100 simulated draws
draws2 <- rbinom(100, size = 10, prob = 0.3)
simplehist(draws2, xlim = c(0, 10))

# 10,000 simulated draws
draws3 <- rbinom(10000, size = 10, prob = 0.3)
simplehist(draws3, xlim = c(0, 10))

#==========================================================


# Plot the probability density function for the
# normal distribution
curve(dnorm(x, mean = 2.5, sd = 1.5), 
      from = -10, to = 10)

# 10 simulated draws from the normal distribution
rnorm(10, mean = 2.5, sd = 1.5)

# Plot the probability density function for the
# Cauchy distribution
curve(dcauchy(x, location = 2.5, scale = 1.5),
      from = -10, to = 10)

# 10 simulated draws from the Cauchy distribution
rcauchy(10, location = 2.5, scale = 1.5)

#==========================================================


# Grid-approximate posterior for 6 successes in 9
# binomial trials
p_grid <- seq(from = 0, to = 1, length.out = 101)
prior <- rep(1, 101)
likelihood <- dbinom(6, size = 9, prob = p_grid)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)

# Plot the grid-approximate posterior
plot(p_grid, posterior)

# Generate 10,000 samples of p from the grid-approximate
# posterior
samples <- sample(p_grid, size = 10000, 
                  replace = TRUE, prob = posterior)

# Plot the samples
plot(samples)
dens(samples, xlim = c(0, 1))

#==========================================================


# Grid-approximate posterior for 5 successes in 5
# binomial trials (leads to an extremely skewed 
# posterior distribution for p)
p_grid2 <- seq(from = 0, to = 1, length.out = 101)
prior2 <- rep(1, 101)
likelihood2 <- dbinom(5, size = 5, prob = p_grid2)
unstd.posterior2 <- likelihood2 * prior2
posterior2 <- unstd.posterior2 / sum(unstd.posterior2)

# Plot the grid-approximate posterior
plot(p_grid2, posterior2)

# Generate 10,000 samples of p from the grid-approximate
# posterior
samples2 <- sample(p_grid2, size = 10000, 
                   replace = TRUE, prob = posterior2)

# Plot the samples
plot(samples2)
dens(samples2, xlim = c(0, 1))

#==========================================================


# Calculate percentile and highest posterior density
# intervals
PI(samples2, prob = 0.5)
HPDI(samples2, prob = 0.5)

PI(samples2, prob = 0.8)
HPDI(samples2, prob = 0.8)

PI(samples2, prob = 1)
HPDI(samples2, prob = 1)

#==========================================================


# Generate a posterior predictive distribution for 
# binomial trials of size 10, given the posterior in
# samples2
preds <- rbinom(10000, size = 10, prob = samples2)
simplehist(preds, xlim = c(0, 10))

# Note, there would be variation in our simulated
# predictions even if we fix the probability of success
# parameter because the data-generating process is
# subject to outcome uncertainty
preds2 <- rbinom(10000, size = 10, prob = 0.8)
simplehist(preds2, xlim = c(0, 10))
