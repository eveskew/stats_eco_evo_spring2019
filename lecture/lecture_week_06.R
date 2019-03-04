

library(rethinking)

#==========================================================


# Gaussian model of Howell height data

# Prepare Howell data for analysis
data(Howell1)
d <- Howell1
d2 <- d[d$age >= 18, ]

# Visualize priors for mean and standard deviation
# parameters
curve(dnorm(x, 178, 20), from = 50, to = 300)
curve(dunif(x, 0, 50), from = -10, to = 60)

# Fit Gaussian model using map()
m4.1 <- map( 
  alist(
    height ~ dnorm(mu, sigma), # likelihood
    mu ~ dnorm(178, 20) , # prior for the mean
    sigma ~ dunif(0, 50) # prior for the standard deviation
  ), 
  data = d2
)

# Summarize model output
precis(m4.1) # shows PIs by default for map() fits

# Get posterior samples
post <- extract.samples(m4.1, n = 10000)

# Plot posterior samples
dens(post$mu) # "marginal" posterior density
dens(post$sigma) # "marginal" posterior density
plot(post$mu, post$sigma)

#==========================================================


# Fit model with predictor (weight) using map()

m4.3 <- map( 
  alist(
    height ~ dnorm(mu, sigma), # likelihood
    mu <- a + b*weight,
    a ~ dnorm(178, 100), # prior for the intercept
    b ~ dnorm(0, 10), # prior for the slope
    sigma ~ dunif(0, 50) # prior for the standard deviation
  ), 
  data = d2
)

# Summarize model output
precis(m4.3)

# Get posterior samples
post <- extract.samples(m4.3, n = 10000)

# Plot posterior samples of a and b parameters
plot(post$a, post$b)

# Why this relationship?
plot(height ~ weight, data = d2, 
     xlim = c(0, 70), ylim = c(100, 200)
)

#==========================================================


# Using a centered predictor variable

# Center the weight predictor variable and compare to the
# raw variable
d2$weight.c <- d2$weight - mean(d2$weight)
plot(weight.c ~ weight, data = d2)
cor(d2$weight, d2$weight.c) # perfect correlation

m4.4 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight.c,
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

precis(m4.4)

# Can also explicitly define parameter start values
# Not necessary in this case, but sometimes needed to 
# help map() accurately describe the posterior shape

m4.4 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight.c,
    a ~ dnorm(178, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d2,
  start = list(a = 100, b = 0, sigma = 2)
)

precis(m4.4)


# To standardize a predictor variable:

d2$weight.s <- 
  (d2$weight - mean(d2$weight)) / sd(d2$weight)
plot(weight.s ~ weight, data = d2)
cor(d2$weight, d2$weight.s) # perfect correlation

# Or:

d2$weight.s <- scale(d2$weight)

#==========================================================


# Visualizing the posterior parameter estimates

# Show the MAP trend line

plot(height ~ weight, data = d2,
     xlim = c(0, 70), ylim = c(100, 200)
)

abline(a = coef(m4.3)["a"], b = coef(m4.3)["b"])

# Show uncertainty in the regression trend line 
# (first 1000 estimates)

head(post)

plot(height ~ weight, data = d2,
     xlim = c(0, 70), ylim = c(100, 200)
)

for (i in 1:1000) {
  abline(a = post$a[i], b = post$b[i],
         col = alpha("darkred", 0.01)
  )
}

#==========================================================


# Generating model-based predictions

# Generate 10,000 predicted heights for an individual
# of 50 kilograms

preds.50 <- rnorm(
  10000,
  mean = post$a + post$b*50,
  sd = post$sigma
)

plot(preds.50)
dens(preds.50)

# Or rethinking has built-in convenience functions to
# generate linear model values (link) or predictions
# (sim) for you

mu.50.rethinking <- 
  link(m4.3, data = list(weight = 50), n = 10000)

dens(mu.50.rethinking)

preds.50.rethinking <-
  sim(m4.3, data = list(weight = 50), n = 10000)

dens(preds.50)
dens(preds.50.rethinking, col = "red", add = TRUE)
