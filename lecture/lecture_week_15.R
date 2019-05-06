

library(rethinking)
library(dplyr)
library(ggplot2)

#==========================================================


# Gaussian process regression

# Load and display Oceanic society distance matrix
data(islandsDistMatrix)
Dmat <- islandsDistMatrix
colnames(Dmat) <- c("Ml", "Ti", "SC", "Ya", "Fi",
                    "Tr", "Ch", "Mn", "To", "Ha")
round(Dmat, 1)

# Load main dataset
data(Kline2)
d <- Kline2
d$society <- 1:10

# Fit the Gaussian process regression
m13.7 <- map2stan(
  alist(
    total_tools ~ dpois(lambda),
    log(lambda) <- a + g[society] + bp*logpop,
    g[society] ~ GPL2(Dmat, etasq, rhosq, 0.01),
    a ~ dnorm(0, 10),
    bp ~ dnorm(0, 1),
    etasq ~ dexp(0.5),
    rhosq ~ dexp(0.5)
  ),
  data = list(
    total_tools = d$total_tools,
    logpop = d$logpop,
    society = d$society,
    Dmat = islandsDistMatrix),
  warmup = 2000, iter = 1e4, chains = 4
)

precis(m13.7, prob = 0.97, depth = 2)


# Visualize the implied covariance functions from the model

post <- extract.samples(m13.7)

# Plot the posterior median covariance function
curve( 
  median(post$etasq)*exp(-median(post$rhosq)*x^2), 
  from = 0, to = 10,
  xlab = "distance (thousand km)", ylab = "covariance", 
  ylim = c(0, 1), yaxp = c(0, 1, 4), lwd = 2
)
# Plot 100 functions sampled from the posterior
for (i in 1:100) {
  curve( 
    post$etasq[i]*exp(-post$rhosq[i]*x^2), 
    add = TRUE, col = col.alpha("black", 0.2)
  )
}

# Plot the posterior median covariance function
curve( 
  median(post$etasq)*exp(-median(post$rhosq)*x^2), 
  from = 0, to = 10,
  xlab = "distance (thousand km)", ylab = "covariance", 
  ylim = c(0, 1), yaxp = c(0, 1, 4), lwd = 2
)
# Plot 1000 functions sampled from the posterior
for (i in 1:1000) {
  curve( 
    post$etasq[i]*exp(-post$rhosq[i]*x^2), 
    add = TRUE, col = col.alpha("black", 0.05)
  )
}


# Generate the median correlation matrix among societies

# Compute posterior median covariance among societies
K <- matrix(0, nrow = 10, ncol = 10)
for (i in 1:10) {
  
  for (j in 1:10) {
    
    K[i,j] <- median(post$etasq) *
  exp(-median(post$rhosq)*islandsDistMatrix[i, j]^2)
  }
}
diag(K) <- median(post$etasq) + 0.01

K

# Convert to correlation matrix
Rho <- round(cov2cor(K), 2)
# Add row/col names for convenience
colnames(Rho) <- c("Ml", "Ti", "SC", "Ya", "Fi",
                   "Tr", "Ch", "Mn", "To", "Ha")
rownames(Rho) <- colnames(Rho)

Rho

#==========================================================


# Generate cat/birdsong data

set.seed(8)
N_days <- 7 # number of observations
alpha <- 20 # average number of birdsong notes w/o cat
beta <- 10 # average number of birdsong notes w cat
cat <- # cat observations
  sample(0:1, size = N_days, 
         prob = c(0.5, 0.5), replace = TRUE)
notes <- # note observations
  rpois(N_days, lambda = (1-cat)*alpha + cat*beta)

dat_list <- list(notes = notes, cat = cat)
dat_list


# Model the data using a Poisson regression

m.cat <- map2stan(
  alist(
    notes ~ dpois(lambda),
    lambda <- (1-cat)*alpha + cat*beta,
    alpha ~ dexp(0.1),
    beta ~ dexp(0.1)
  ),
  data = dat_list,
  chains = 4
)

rstan::traceplot(m.cat@stanfit)

precis(m.cat, prob = 0.97)

#==========================================================


# Generate cat/birdsong data with missingness

cat_obs <- cat
cat_obs[c(1, 4)] <- -1

stan_dat_list <- 
  list(notes = notes, cat = cat_obs, N = N_days)
stan_dat_list


# Model the data with missingness

mcatmiss_code <- "
data{
int N;
int notes[N];
int cat[N];
}
parameters{
real<lower=0,upper=1> kappa;
real<lower=0> beta;
real<lower=0> alpha;
}
model{
beta ~ exponential( 0.1 );
alpha ~ exponential( 0.1 );
kappa ~ beta( 4 , 4 );
for ( i in 1:N ) {
if ( cat[i]==-1 ) { // cat missing
target += log_mix( kappa ,
poisson_lpmf( notes[i] | beta ),
poisson_lpmf( notes[i] | alpha )
);
} else { // cat not missing
cat[i] ~ bernoulli(kappa);
notes[i] ~ poisson( (1-cat[i])*alpha + cat[i]*beta );
} 
}//i
}//model
generated quantities{
vector[N] cat_impute;
for ( i in 1:N ) {
real logPxy;
real logPy;
if ( cat[i]==-1 ) {
// need P(x|y)
// P(x|y) = P(x,y)/P(y)
// P(x,y) = P(x)P(y|x)
// P(y) = P(x==1)P(y|x==1) + P(x==0)P(y|x==0)
logPxy = log(kappa) + poisson_lpmf(notes[i]|beta);
logPy = log_mix( kappa ,
poisson_lpmf( notes[i] | beta ),
poisson_lpmf( notes[i] | alpha ) );
cat_impute[i] = exp( logPxy - logPy );
} else {
cat_impute[i] = cat[i];
}
}//i
}//gq
"

m.cat.miss <- 
  rstan::stan(model_code = mcatmiss_code, 
              data = stan_dat_list, 
              iter = 10000, chains = 4)

rstan::traceplot(m.cat.miss)

precis(m.cat.miss, prob = 0.97, depth = 2)

# Extract posterior samples
e <- extract(m.cat.miss)

# Plot the posterior distribution for the probability of
# presence for the two missing cat observations
dens(e$cat_impute[, 1], 
     xlab = "probability of cat presence at time 1")
dens(e$cat_impute[, 4],
     xlab = "probability of cat presence at time 4")

# Plot cat observation versus bird singing rate
stan_dat_list %>%
  as.data.frame() %>%
  ggplot(aes(x = as.factor(cat), y = notes, 
             color = as.factor(cat))) +
  geom_point() +
  xlab("cat") +
  scale_x_discrete(
    labels = c("missing data", "absent", "present")
  ) +
  theme_minimal() +
  theme(legend.position = "none")
