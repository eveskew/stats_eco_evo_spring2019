

library(rethinking)

#==========================================================


# Load the reed frog data
data(reedfrogs)
d <- reedfrogs

# Make the tank cluster variable
d$tank <- 1:nrow(d)

str(d)

# Plot the observed tank-level survival proportions
plot(propsurv ~ tank, data = d, ylim = c(0, 1))

#==========================================================


# Fit a binomial GLM predicting frog survival using a
# tank-level index variable
m12.1 <- map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(0, 5)
  ),
  data = d
)

precis(m12.1, prob = 0.97)
# Now need to use the "depth" argument to show all fit
# model parameters...
precis(m12.1, depth = 2, prob = 0.97)

# And when working with posterior samples, we have to
# deal with new data structures...
post <- extract.samples(m12.1, n = 1000)
str(post)
is.list(post)
is.matrix(post$a_tank)

# To get the posterior samples for the first tank:
post$a_tank[, 1]
# To get the posterior samples for the second tank:
post$a_tank[, 2]

# Plot the implied probability of survival for the first
# three tanks
dens(logistic(post$a_tank[, 1]), 
     xlim = c(0, 1), ylim = c(0, 10))

dens(logistic(post$a_tank[, 2]), 
     xlim = c(0, 1), ylim = c(0, 100))

dens(logistic(post$a_tank[, 3]), 
     xlim = c(0, 1), ylim = c(0, 10))

#==========================================================


# Fit a binomial GLMM predicting frog survival using a
# tank-level index variable
m12.2 <- map2stan(
  alist(
    surv ~ dbinom(density, p),
    logit(p) <- a_tank[tank],
    a_tank[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1)
  ), 
  data = d, 
  iter = 4000, 
  chains = 4
)

precis(m12.2, prob = 0.97)
precis(m12.2, depth = 2, prob = 0.97)


# Visualization to demonstrate that the tank-level
# intercepts are being drawn from (i.e., constrained by)
# a Gaussian distribution

# Extract posterior samples
post <- extract.samples(m12.2, n = 4000)

# Generate an x-axis sequence for plotting
x.seq <- seq(from = -6, to = 6, by = 0.01)

# Plot the Gaussian distribution implied by the posterior
# means for the "a" and "sigma" parameters
plot(x.seq, dnorm(x.seq, 
                  mean = coef(m12.2)["a"], 
                  sd = coef(m12.2)["sigma"]), 
     xlab = "log-odds of survival", ylab = "Density",
     type = "n"
)
      
lines(x.seq, dnorm(x.seq, 
                   mean = coef(m12.2)["a"], 
                   sd = coef(m12.2)["sigma"])
)

# Plot as vertical lines, with some time lag, the posterior
# means for the tank-level intercepts 
for (i in 1:48) {
  
  Sys.sleep(time = 0.5)
  abline(v = coef(m12.2)[i], lty = 1, col = "red")
}

#==========================================================


# How do the tank-level intercept estimates from the GLM
# and the GLMM compare?
plot(coef(m12.1), coef(m12.2)[1:48],
     xlim = c(-3, 7), ylim = c(-3, 7),
     xlab = "Tank-level intercept mean est. from GLM",
     ylab = "Tank-level intercept mean est. from GLMM")
abline(a = 0, b = 1, lty = 2)
