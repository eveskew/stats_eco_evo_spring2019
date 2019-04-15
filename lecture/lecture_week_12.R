

library(rethinking)
library(dplyr)

#==========================================================


# Generate a vector of probability values
probabilities <- seq(from = 0, to = 1, by = 0.01)
probabilities

# Compute the odds for each of these probabilities and 
# plot the relationship
odds <- probabilities / (1 - probabilities)
plot(probabilities, odds, type = "n")
lines(probabilities, odds)

# Compute the log-odds for each of these probabilities 
# and plot the relationship
log_odds <- log(odds)
plot(probabilities, log_odds, type = "n")
lines(probabilities, log_odds)

# Convert the log-odds values back to probabilities
# using the logistic function
log_odds_to_probs <- logistic(log_odds)
plot(probabilities, log_odds_to_probs, type = "n")
lines(probabilities, log_odds_to_probs)

#==========================================================


# Chimpanzee prosociality binomial models

# Import the chimpanzee data
data(chimpanzees)
d <- chimpanzees

# Fit an intercept-only binomial GLM
m10.1 <- map(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a,
    a ~ dnorm(0, 10)
  ),
  data = d)

precis(m10.1, prob = 0.97)

# Consider the fit model parameter
exp(0.32) # on the odds scale
logistic(0.32) # on the probability scale

# Fit a more complex binomial GLM considering the effects
# of prosocial and condition treatments
m10.3 <- map(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a + bP*prosoc_left + bPC*prosoc_left*condition,
    a ~ dnorm(0, 10) ,
    bP ~ dnorm(0, 10) ,
    bPC ~ dnorm(0, 10)
  ),
  data = d)

precis(m10.3, prob = 0.97)


# Visualize predictions from model m10.3

# Dummy data for predictions across treatments
d.pred <- data.frame(
  prosoc_left = c(0, 1, 0, 1), # right/left/right/left
  condition = c(0, 0, 1, 1) # control/control/partner/partner
)

# Build predictions for probability of success using
# "link()"
preds.p <- link(m10.3, data = d.pred)

# Summarize the probability prediction values
preds.p.mean <- apply(preds.p, 2, mean)
preds.p.PI <- apply(preds.p, 2, PI, prob = 0.9)

# Generate an empty plot frame with good axes
plot(0, 0, type = "n", xaxt = "n",
     xlab = "prosoc_left/condition",
     ylab = "proportion pulled left",
     xlim = c(1, 4), ylim = c(0, 1))
axis(1, at = 1:4, labels = c("0/0", "1/0", "0/1", "1/1"))

# Plot raw data, one trend for each of 7 individual
# chimpanzees using "by()"
p <- by(d$pulled_left,
        list(d$prosoc_left, d$condition, d$actor), mean)
for (chimp in 1:7)
  lines(1:4, as.vector(p[, , chimp]), 
        col = rangi2, lwd = 1.5)

# Superimpose posterior predictions
lines(1:4, preds.p.mean)
shade(preds.p.PI, 1:4)


# Replicate model m10.3 using aggregated binomial data

# Generate data in an aggregated form
d.aggregated <- d %>%
  group_by(prosoc_left, condition) %>%
  summarize(
    pulled_left_aggregated = sum(pulled_left),
    n_trials = n()
  ) %>%
  data.frame()

d.aggregated

# Fit the model using aggregated data
m10.5 <- map(
  alist(
    pulled_left_aggregated ~ dbinom(n_trials, p),
    logit(p) <- a + bP*prosoc_left + bPC*prosoc_left*condition,
    a ~ dnorm(0, 10),
    bP ~ dnorm(0, 10),
    bPC ~ dnorm(0, 10)
  ),
  data = d.aggregated)

# Compare model output
precis(m10.5, prob = 0.97)
precis(m10.3, prob = 0.97)

#==========================================================


# Snow goose color binomial models

# Load in hypothetical snow goose data in aggregated
# binomial format
geese <- data.frame(
  blue_geese = c(215, 84, 7),
  total_geese = c(500, 300, 25),
  study_site = c("1", "2", "3")
)

# Add on a variable indicating the proportion of blue
# morphs at each study site
geese$prop_blue <- geese$blue_geese/geese$total_geese

# Plot the raw proportions of blue morphs
plot(prop_blue ~ study_site, data = geese, 
     ylim = c(0, 0.5))

# Generate dummy variables for site affiliation
geese$site2 <- ifelse(geese$study_site == "2", 1, 0)
geese$site3 <- ifelse(geese$study_site == "3", 1, 0)

# Fit a binomial GLM using site to predict the 
# probability of a goose being the blue morph
goose.model <- map(
  alist(
    blue_geese ~ dbinom(size = total_geese, prob = p),
    logit(p) ~ a + b_site2*site2 + b_site3*site3,
    a ~ dnorm(0, 10),
    b_site2 ~ dnorm(0, 10),
    b_site3 ~ dnorm(0, 10)
  ),
  data = geese
)

precis(goose.model, prob = 0.97)


# Visualize model inference

# Extract samples from the model posterior
goose.post <- extract.samples(goose.model, n = 10000)

# Show the posterior distribution of the intercept
# parameter (on the log-odds scale), which corresponds
# to study site 1
dens(goose.post$a)
# Show the posterior distribution of the implied 
# probability of blue morphs at study site 1
dens(logistic(goose.post$a))


# Plot the log-odds of a goose being blue by plotting
# posterior parameter samples

# site 1 (intercept or reference category)
dens(goose.post$a, xlim = c(-3, 1),
     xlab = "log-odds of a blue goose")
# site 2 
dens(goose.post$a + goose.post$b_site2, 
     add = TRUE, col = "blue")
# site 3
dens(goose.post$a + goose.post$b_site3, 
     add = TRUE, col = "green")

# Plot the implied probability of a goose being blue by 
# plotting posterior parameter samples transformed through
# the logistic function

# site 1 (intercept or reference category)
dens(logistic(goose.post$a), xlim = c(0, 0.5),
     xlab = "implied probability of a blue goose")
# site 2 
dens(logistic(goose.post$a + goose.post$b_site2), 
     add = TRUE, col = "blue")
# site 3
dens(logistic(goose.post$a + goose.post$b_site3), 
     add = TRUE, col = "green")


# You can do this same type of prediction plot 
# using "link()"

counterfactual.site1 <- data.frame(site2 = 0, site3 = 0)
counterfactual.site2 <- data.frame(site2 = 1, site3 = 0)
counterfactual.site3 <- data.frame(site2 = 0, site3 = 1)

probs.site1 <- link(goose.model, counterfactual.site1)
probs.site2 <- link(goose.model, counterfactual.site2)
probs.site3 <- link(goose.model, counterfactual.site3)

dens(probs.site1, xlim = c(0, 0.5),
     xlab = "implied probability of a blue goose")
dens(probs.site2, 
     add = TRUE, col = "blue")
dens(probs.site3, 
     add = TRUE, col = "green")
