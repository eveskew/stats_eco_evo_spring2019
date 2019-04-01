

library(rethinking)

#==========================================================


# Import and prep the country data

data(rugged)
d <- rugged

# Make log version of outcome (GDP in year 2000)
d$log_gdp <- log(d$rgdppc_2000)

# Extract countries with GDP data
dd <- d[complete.cases(d$rgdppc_2000), ]

# Split countries into African and non-African nations
d.A1 <- dd[dd$cont_africa == 1, ] # Africa
d.A0 <- dd[dd$cont_africa == 0, ] # not Africa

#==========================================================


# Fit models for country subsets separately (for later
# comparison with model fit on all data)

# African nations

m7.1 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d.A1)

# non-African nations

m7.2 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d.A0)

#==========================================================


# Fit a linear model with all data, using terrain 
# ruggedness to predict GDP

m7.3 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = dd)

precis(m7.3, prob = 0.97)


# Plot model predictions

rugged.seq <- seq(from = -1, to = 8, by = 0.25)

counterfactual <- data.frame(rugged = rugged.seq)

mu <- link(m7.3, data = counterfactual)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.97)

plot(log(rgdppc_2000) ~ rugged, data = dd,
     xlab = "Terrain Ruggedness Index",
     ylab = "log(GDP year 200)",
     col = "red4"
)
mtext("Linear regression", 3)
lines(rugged.seq, mu.mean, col = "red4")
shade(mu.PI, rugged.seq, col = col.alpha("red4", 0.3))


# Demonstrate that map() doesn't care how you specify
# the mean parameter

m7.3.alt <- map(
  alist(
    log_gdp ~ dnorm(a + bR*rugged, sigma),
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = dd)

precis(m7.3.alt, prob = 0.97)
precis(m7.3, prob = 0.97)

#==========================================================


# Fit a multiple regression model using terrain ruggedness 
# and continent identity to predict GDP

m7.4 <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa,
    a ~ dnorm(8, 100),
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = dd)

precis(m7.4, prob = 0.97)


# Plot model predictions

counterfactual.Africa <- 
  data.frame(cont_africa = 1, rugged = rugged.seq)
counterfactual.NotAfrica <-
  data.frame(cont_africa = 0, rugged = rugged.seq)

mu.Africa <- link(m7.4, data = counterfactual.Africa)
mu.Africa.mean <- apply(mu.Africa, 2, mean)
mu.Africa.PI <- apply(mu.Africa, 2, PI, prob = 0.97)

mu.NotAfrica <- link(m7.4, data = counterfactual.NotAfrica)
mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)
mu.NotAfrica.PI <- apply(mu.NotAfrica, 2, PI, prob = 0.97)

plot(log(rgdppc_2000) ~ rugged, data = dd,
     xlab = "Terrain Ruggedness Index",
     ylab = "log(GDP year 200)",
     col = ifelse(dd$cont_africa == 1, rangi2, "black")
)
mtext("Multiple regression, no interaction", 3)
lines(rugged.seq, mu.Africa.mean, col = rangi2)
shade(mu.Africa.PI, rugged.seq, 
      col = col.alpha(rangi2, 0.3))
lines(rugged.seq, mu.NotAfrica.mean)
shade(mu.NotAfrica.PI, rugged.seq)

#==========================================================


# Fit a multiple regression with an interaction term

m7.5b <- map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm(8, 100),
    bA ~ dnorm(0, 1),
    bR ~ dnorm(0, 1),
    bAR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = dd)

precis(m7.5b, prob = 0.97)


# Generate model-based predictions

mu.Africa <- link(m7.5b, data = counterfactual.Africa)
mu.Africa.mean <- apply(mu.Africa, 2, mean)
mu.Africa.PI <- apply(mu.Africa, 2, PI, prob = 0.97)

mu.NotAfrica <- link(m7.5b, data = counterfactual.NotAfrica)
mu.NotAfrica.mean <- apply(mu.NotAfrica, 2, mean)
mu.NotAfrica.PI <- apply(mu.NotAfrica, 2, PI, prob = 0.97)


# Generate a prediction plot following the book

par(mfrow = c(1, 2))
    
# plot African nations with regression
plot(log(rgdppc_2000) ~ rugged, data = d.A1,
     xlab = "Terrain Ruggedness Index",
     ylab = "log(GDP year 200)",
     col = rangi2
)
mtext("African nations" , 3 )
lines(rugged.seq, mu.Africa.mean, col = rangi2)
shade(mu.Africa.PI, rugged.seq, 
      col = col.alpha(rangi2, 0.3))

# plot non-African nations with regression
plot(log(rgdppc_2000) ~ rugged, data = d.A0,
     xlab = "Terrain Ruggedness Index",
     ylab = "log(GDP year 200)",
     col = "black"
)
mtext("Non-African nations", 3)
lines(rugged.seq, mu.NotAfrica.mean)
shade(mu.NotAfrica.PI, rugged.seq)


# Generate an alternate prediction plot

par(mfrow = c(1, 1))

plot(log(rgdppc_2000) ~ rugged, data = dd,
     xlab = "Terrain Ruggedness Index",
     ylab = "log(GDP year 200)",
     col = ifelse(dd$cont_africa == 1, rangi2, "black")
)
mtext("Multiple regression, with interaction", 3)  
lines(rugged.seq, mu.Africa.mean, col = rangi2)
shade(mu.Africa.PI, rugged.seq, 
      col = col.alpha(rangi2, 0.3))
lines(rugged.seq, mu.NotAfrica.mean)
shade(mu.NotAfrica.PI, rugged.seq)


# Compare model output with models fit on data subsets

precis(m7.1)
precis(m7.2)
precis(m7.5b)
