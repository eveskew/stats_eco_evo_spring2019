

library(rethinking)

#==========================================================


# Linear regression of milk energy data

# Single predictor: neocortex percentage (np)


# Prepare milk energy data for analysis
data(milk)
d <- milk
dcc <- d[complete.cases(d), ]

# Visualize priors for intercept, slope, and standard
# deviation parameters
curve(dnorm(x, 0, 100), from = -300, to = 300)
curve(dnorm(x, 0, 1), from = -10, to = 10)
curve(dunif(x, 0, 1), from = -5, to = 5)

# Fit linear regression using map()
m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma), # likelihood
    mu <- a + bn*neocortex.perc,
    a ~ dnorm(0, 100), # prior for the intercept
    bn ~ dnorm(0, 1), # prior for the np effect
    sigma ~ dunif(0, 1) # prior for the standard deviation
  ),
  data = dcc
)

# Summarize model output
precis(m5.5, digits = 3)

# Get posterior samples
post <- extract.samples(m5.5, n = 10000)

# Plot posterior samples
dens(post$a)
dens(post$bn) 
plot(post$a, post$bn) # correlation of intercept and slope


# Counterfactual plot


# Generate counterfactual data
np.seq <- 0:100 # sequence of np values to use
pred.data <- data.frame(neocortex.perc = np.seq)

# Generate values for Gaussian mean
mu <- link(m5.5, data = pred.data, n = 10000)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.95)

# Plotting
plot(kcal.per.g ~ neocortex.perc, 
     data = dcc, col = "cornflowerblue")
lines(np.seq, mu.mean)
shade(mu.PI, np.seq)

# Plotting (beyond range of data)
plot(kcal.per.g ~ neocortex.perc, 
     data = dcc, col = "cornflowerblue",
     xlim = c(0, 100), ylim = c(0, 1))
lines(np.seq, mu.mean)
shade(mu.PI, np.seq)
# illustrates that prediction is highly uncertain beyond
# observed range of data

#==========================================================


# Linear regression of milk energy data

# Single predictor: log body mass


# Prepare milk energy data for analysis
dcc$log.mass <- log(dcc$mass)

# Fit linear regression using map()
m5.6 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bm*log.mass,
    a ~ dnorm(0, 100), # prior for the intercept
    bm ~ dnorm(0, 1), # prior for the body mass effect
    sigma ~ dunif(0, 1) # prior for the standard deviation
  ),
  data = dcc
)

# Summarize model output
precis(m5.6, digits = 3)

# Get posterior samples
post <- extract.samples(m5.6, n = 10000)

# Plot posterior samples
dens(post$a)
dens(post$bm) 
plot(post$a, post$bm) # correlation of intercept and slope


# Counterfactual plot


# Generate counterfactual data
lbm.seq <- seq(from = -5, to = 5, length.out = 100)
pred.data <- data.frame(log.mass = lbm.seq)

# Generate values for Gaussian mean
mu <- link(m5.6, data = pred.data, n = 10000)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.95)

# Plotting
plot(kcal.per.g ~ log.mass, 
     data = dcc, col = "cornflowerblue")
lines(lbm.seq, mu.mean)
shade(mu.PI, lbm.seq)

# Plotting (beyond range of data)
plot(kcal.per.g ~ log.mass, 
     data = dcc, col = "cornflowerblue",
     xlim = c(-5, 5), ylim = c(0, 1))
lines(lbm.seq, mu.mean)
shade(mu.PI, lbm.seq)

#==========================================================


# Multiple regression of milk energy data

# Two predictors: neocortex percentage and log body mass


# Fit linear regression using map()
m5.7 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn*neocortex.perc + bm*log.mass,
    a ~ dnorm(0, 100), # prior for the intercept
    bn ~ dnorm(0, 1), # prior for the np effect
    bm ~ dnorm(0, 1), # prior for the body mass effect
    sigma ~ dunif(0, 1) # prior for the standard deviation
  ),
  data = dcc
)

# Summarize model output
precis(m5.7, digits = 3)

# Get posterior samples
post <- extract.samples(m5.7, n = 10000)

# Plot posterior samples
dens(post$a)
dens(post$bn)
dens(post$bm)


# Counterfactual plots


# For neocortex percentage
mean.log.mass <- mean(dcc$log.mass)
pred.data <- 
  data.frame(neocortex.perc = np.seq,
             log.mass = mean.log.mass)

mu <- link(m5.7, data = pred.data, n = 10000)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.95)

plot(kcal.per.g ~ neocortex.perc, 
     data = dcc, type = "n")
lines(np.seq, mu.mean)
shade(mu.PI, np.seq)

# For log body mass
mean.neocortex.perc <- mean(dcc$neocortex.perc)
pred.data <- 
  data.frame(neocortex.perc = mean.neocortex.perc,
             log.mass = lbm.seq)

mu <- link(m5.7, data = pred.data, n = 10000)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.95)

plot(kcal.per.g ~ log.mass, 
     data = dcc, type = "n")
lines(lbm.seq, mu.mean)
shade(mu.PI, lbm.seq)

#==========================================================


# Computing variable residuals and using these to fit
# multiple bivariate regressions


# Residual neocortex percentage, after accounting for
# log body mass

plot(neocortex.perc ~ log.mass, data = dcc,
     col = "cornflowerblue")

# Using lm() as a shortcut to visualize best fit line 
# and compute residuals
fit <- lm(neocortex.perc ~ log.mass, data = dcc)
abline(fit)
mu <- coef(fit)[1] + coef(fit)[2]*dcc$log.mass
dcc$np.residual <- resid(fit)

for (i in 1:length(dcc$np.residual)) {
  
  x <- dcc$log.mass[i] # x location of line segment
  y <- dcc$neocortex.perc[i] # observed endpoint of line segment
  # draw the line segments
  lines(c(x, x), c(mu[i], y), 
        lwd = 0.8, col = col.alpha("black", 0.8))
}

# Fit a linear regression using neocortex percentage
# RESIDUALS as a predictor of milk energy
m.np.residual <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bnr*np.residual,
    a ~ dnorm(0, 100),
    bnr ~ dnorm(0, 1),
    sigma ~ dunif(0, 1) 
  ),
  data = dcc
)


# Residual log body mass, after accounting for neocortex
# percentage

plot(log.mass ~ neocortex.perc, data = dcc,
     col = "cornflowerblue")

# Using lm() as a shortcut to visualize best fit line 
# and compute residuals
fit <- lm(log.mass ~ neocortex.perc, data = dcc)
abline(fit)
mu <- coef(fit)[1] + coef(fit)[2]*dcc$neocortex.perc
dcc$lbm.residual <- resid(fit)

for (i in 1:length(dcc$lbm.residual)) {
  
  x <- dcc$neocortex.perc[i] # x location of line segment
  y <- dcc$log.mass[i] # observed endpoint of line segment
  # draw the line segments
  lines(c(x, x), c(mu[i], y), 
        lwd = 0.8, col = col.alpha("black", 0.8))
}

# Fit a linear regression using log body mass RESIDUALS
# as a predictor of milk energy
m.lbm.residual <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bmr*lbm.residual,
    a ~ dnorm(0, 100),
    bmr ~ dnorm(0, 1),
    sigma ~ dunif(0, 1) 
  ),
  data = dcc
)


# Compare fit models using residual predictor variables
# with the multiple regression using both original
# variables
precis(m.np.residual)
precis(m.lbm.residual)
precis(m5.7)

#==========================================================


# Linear regression with a categorical predictor variable


# Import Howell data
data(Howell1)
d <- Howell1
str(d)
d$male

# In this case, "male" is already a dummy variable, but we
# could easily create it with an "ifelse()" statement if
# it was a string variable (see book for examples)

# Fit a linear regression using sex as a predictor of 
# height
m5.15 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bm*male,
    a ~ dnorm(178, 100),
    bm ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d
)

# Summarize the fit model
precis(m5.15)

# Generate posterior samples
post <- extract.samples(m5.15, n = 10000)

# To visualize the expected mean height value for females
dens(post$a, xlim = c(120, 160))

# To visualize the expected mean height value for males
dens(post$a + post$bm, col = "darkred", add = TRUE)
