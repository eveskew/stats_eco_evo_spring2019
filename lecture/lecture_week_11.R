

library(rethinking)
library(dplyr)

#==========================================================


# Import and prep the country data

data(rugged)
d <- rugged

# Make log version of outcome (GDP in year 2000)
d$log_gdp <- log(d$rgdppc_2000)

# Extract countries with GDP data
dd <- d[complete.cases(d$rgdppc_2000), ]

#==========================================================


# Fit a multiple regression with an interaction term

m8.1 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm(0, 100),
    bA ~ dnorm(0, 10),
    bR ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ),
  data = dd)

precis(m8.1, prob = 0.97)


# Fit a multiple regression with an interaction term
# using map2stan()

# Trim data to ensure we don't run into any Stan issues
dd.trim <- dd %>%
  select(log_gdp, rugged, cont_africa)

# Fit the model using map2stan()
m8.1stan <- map2stan(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm(0, 100),
    bA ~ dnorm(0, 10),
    bR ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 2)
  ),
  data = dd.trim)

# Compare model fits using precis()
precis(m8.1stan, prob = 0.97)
precis(m8.1, prob = 0.97)

# Other model summary functions
show(m8.1stan)
summary(m8.1stan)

#==========================================================


# Plot parameter trace plots

# Using the rethinking plotting method
plot(m8.1stan)
plot(m8.1stan, window = c(1000, 2000)) # modify x-axis
# Using the rstan plotting method
rstan::traceplot(m8.1stan@stanfit, 
                 pars = c("a", "bR", "bA", "bAR", "sigma"))

#==========================================================


# A reminder about the Poisson distribution

# To generate random samples from the Poisson:
rpois(n = 10, lambda = 5)

# The probability mass function:
dpois(0, lambda = 5)
dpois(3, lambda = 5)
dpois(5, lambda = 5)
dpois(20, lambda = 5)

#==========================================================


# Fitting a Poisson GLM


# Import the Kline dataset
data(Kline)
d <- Kline
d

# Generate modified predictor variables
d$log_pop <- log(d$population)
d$contact_high <- ifelse(d$contact == "high", 1, 0)

# Fit a Poisson GLM with the effects of log population
# size, contact rate, and their interaction
m10.10 <- map(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + bp*log_pop + bc*contact_high + bpc*log_pop*contact_high,
    a ~ dnorm(0, 100),
    bp ~ dnorm(0, 1),
    bc ~ dnorm(0, 1),
    bpc ~ dnorm(0, 1)
  ),
  data = d
)

precis(m10.10, prob = 0.97)

#==========================================================


# Generate model-based predictions (the manual way)

# Get 5,000 posterior samples from the fit model
post <- extract.samples(m10.10, n = 5000)

# Generate expected lambda values for an island with a
# log population size of 8 and high contact rate by
# applying the inverse link function to the linear model
# for lambda
lambda.high <- 
  exp(post$a + post$bp*8 + post$bc*1 + post$bpc*8*1)

# Generate expected lambda values for an island with a
# log population size of 8 and low contact rate by
# applying the inverse link function to the linear model
# for lambda
lambda.low <- 
  exp(post$a + post$bp*8 + post$bc*0 + post$bpc*8*0)

# Remember, these represent the expected values
# for lambda, NOT the actual counts
head(lambda.high)
head(lambda.low)

# To get actual count predictions, we need to feed these
# lambda values through rpois()
preds.high <- rpois(n = 5000, lambda.high)
preds.low <- rpois(n = 5000, lambda.low)

head(preds.high)
head(preds.low)


# Or we could generate the same predictions by using the
# convenience function sim(), feeding it counterfactual
# data

# Generate counterfactual data for high and low contact
# islands
counterfactual.high <- 
  data.frame(log_pop = 8, contact_high = 1)
counterfactual.low <- 
  data.frame(log_pop = 8, contact_high = 0)

# Generate predictions using sim()
preds.high.sim <- 
  sim(m10.10, n = 5000, data = counterfactual.high)
preds.low.sim <- 
  sim(m10.10, n = 5000, data = counterfactual.low)

head(preds.high.sim)
head(preds.low.sim)


# Visualize the predictions

par(mfrow = c(2, 2))

simplehist(preds.high, 
           xlab = "", ylab = "",
           xlim = c(0, 60), ylim = c(0, 500), 
           col = "red", 
           main = "High contact preds, manual")

simplehist(preds.high.sim, 
           xlab = "", ylab = "",
           xlim = c(0, 60), ylim = c(0, 500), 
           col = alpha("red", 0.5), 
           main = "High contact preds, sim")

simplehist(preds.low, 
           xlab = "", ylab = "",
           xlim = c(0, 60), ylim = c(0, 500), 
           col = "blue", 
           main = "Low contact preds, manual")

simplehist(preds.low.sim, 
           xlab = "", ylab = "",
           xlim = c(0, 60), ylim = c(0, 500), 
           col = alpha("blue", 0.5), 
           main = "Low contact preds, sim")

#==========================================================


# Fit the same Poisson GLM using map2stan() with 
# 4 Markov chains
m10.10stan <- map2stan(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + bp*log_pop + bc*contact_high + bpc*log_pop*contact_high,
    a ~ dnorm(0, 100),
    bp ~ dnorm(0, 1),
    bc ~ dnorm(0, 1),
    bpc ~ dnorm(0, 1)
  ),
  data = d, 
  chains = 4
)

# Compare model fits using precis()
precis(m10.10stan, prob = 0.97)
precis(m10.10, prob = 0.97)

# Plot parameter trace plots
plot(m10.10stan)
plot(m10.10stan, window = c(500, 2000))
plot(m10.10stan, window = c(1000, 2000))
rstan::traceplot(m10.10stan@stanfit, 
                 pars = c("a", "bp", "bc", "bpc"))
