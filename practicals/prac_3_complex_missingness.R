# An R script which fits some MNAR-type models using JAGS

# If you have forgotten how to use JAGS go back to prac_2_jags_mda.R for details

# Packages we will use
library(mice)
library(R2jags)
library(nlme)

# Part 1 - logistic regression models -------------------------------------

# Code for a logistic regression model with one covariate
model_code = '
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dbern(p[i])
    logit(p[i]) = alpha + beta * x[i]
  }
  # Priors
  alpha ~ dnorm(0, 5^-2)
  beta ~ dnorm(0, 5^-2)
}
'

# Fit this to the nhanes data using whether chl is missing or not
y = as.integer(is.na(nhanes$chl))
x = nhanes$age

# Jitter the plot so the data doesn't over-plot
plot(jitter(x, 0.1),jitter(y, 0.1))

# Parameters to watch
mod_par =  c("p", "alpha", "beta")

# Run the model
model_run = jags(data = list(N = length(y), y = y, x = x),
                 parameters.to.save = mod_par,
                 model.file = textConnection(model_code))

# Plot the output - check the Rhat is close to 1 (i.e. converged) 
# Then look at the parameter values
plot(model_run)

# Create a trace plot of the values - should look stationary
post = model_run$BUGSoutput$sims.list
plot(post$alpha, type = 'l')


# Part 2 longitudinal data -------------------------------------------------

# We wil use the Orthodont data:
help(Orthodont)
head(Orthodont)
plot(Orthodont)

# Code for a regression model with a random effect for intercept
model_code = '
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dnorm(alpha[person[i]] + beta * (age[i] - mean(age)), sigma^-2)
  }
  # Prior for intercept
  for(j in 1:N_people) {
    alpha[j] ~ dnorm(mu_alpha, sigma_alpha^-2)
  }
  # Priors on other parameters
  mu_alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 10^-2)
  sigma ~ dgamma(1,1)
  sigma_alpha ~ dgamma(1,1)
}
'

# Set up parameters and run
mod_par =  c("mu_alpha", "alpha", "beta", "sigma", "sigma_alpha")
model_run = jags(data = 
                   list(N = nrow(Orthodont), 
                        N_people = length(unique(Orthodont$Subject)),
                        y = Orthodont$distance, 
                        age = Orthodont$age,
                        person = Orthodont$Subject),
                 parameters.to.save = mod_par,
                 model.file = textConnection(model_code))


# Check Rhat and look at parameters
plot(model_run)


# Longitudinal with missing data ------------------------------------------

# Create some MCAR NAs in it
set.seed(123)
Orthodont2 = Orthodont
Orthodont2$distance[sample(1:nrow(Orthodont2), 40)] = NA
Orthodont2$age[sample(1:nrow(Orthodont2), 20)] = NA

# This plot should now have gaps
plot(Orthodont2)

# The jags code now including the missingness prior
model_code = '
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dnorm(alpha[person[i]] + beta * (age[i] - mean(age)), sigma^-2)
  }
  # Prior for intercept
  for(j in 1:N_people) {
    alpha[j] ~ dnorm(mu_alpha, sigma_alpha^-2)
  }
  # Prior for missing values
  for(k in 1:N_miss_age) {
    age[miss[k]] ~ dunif(min_age, max_age)
  }
  # Priors on other parameters
  mu_alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 10^-2)
  sigma ~ dgamma(1,1)
  sigma_alpha ~ dgamma(1,1)
}
'

# Run the model - needs quite a log of extra data!
mod_par =  c("mu_alpha", "alpha", "beta", "sigma", 
             "sigma_alpha", "y", "age")
model_run = jags(data = 
                   list(N = nrow(Orthodont2), 
                        N_people = length(unique(Orthodont2$Subject)),
                        N_miss_age = sum(is.na(Orthodont2$age)),
                        miss = which(is.na(Orthodont2$age)),
                        y = Orthodont2$distance, 
                        age = Orthodont2$age,
                        min_age = min(Orthodont2$age, na.rm = TRUE),
                        max_age = max(Orthodont2$age, na.rm = TRUE),
                        person = Orthodont2$Subject),
                 parameters.to.save = mod_par,
                 model.file = textConnection(model_code))

# Check Rhat and parameter values - especially the missing ones
plot(model_run)


# MNAR - selection model --------------------------------------------------

# JAGS code for an MNAR selection model
model_code = '
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dnorm(alpha + beta * x[i], sigma^-2)
    m[i] ~ dbern(p[i])
    logit(p[i]) = gamma + omega * (y[i] - mean(y))
  }
  # Priors for regression model
  alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 100^-2)
  sigma ~ dgamma(1, 1)
  # Priors for missingness model
  gamma ~ dnorm(0, 5^-2)
  omega ~ dnorm(0, 5^-2)
}
'

# Create some simulated data for this
set.seed(123)
N = 100
x = sort(runif(N))
alpha = 3
beta = 2
sigma = 0.2
y_true = rnorm(N, alpha + beta * x, sigma)
gamma = -0.5
omega = 1
m = rbinom(N, 1, plogis(gamma + omega * (y_true - mean(y_true))))
y = y_true
y[m==0] = NA

# Plot the simulation
plot(x, y_true)
points(x, y, col = 'red', pch = 19)

# Run the model on the simulated data
mod_par =  c("alpha", "beta", "sigma", "gamma", "omega", "y")
model_run = jags(data = list(N = N, y = y, x = x, m = m),
                 parameters.to.save = mod_par,
                 model.file = textConnection(model_code))

# Check R-hat and parameters
plot(model_run)

# Seeing as we know the truth we can plot the histograms against the truth
pars = model_run$BUGSoutput$sims.list
par(mfrow=c(2,2))
hist(pars$alpha, breaks = 30, main = 'Intercept'); abline(v = alpha, col='red')
hist(pars$beta, breaks = 30, main = 'Slope'); abline(v = beta, col='red')
hist(pars$gamma, breaks = 30, main = 'Miss intercept'); abline(v = gamma, col='red')
hist(pars$omega, breaks = 30, main = 'Miss coef of y'); abline(v = omega, col='red')
par(mfrow=c(1,1))



# MNAR Pattern Mixture Model ----------------------------------------------

# Some code to fit a pattern mixture model
model_code = '
model
{
  # Likelihood
  for (i in 1:N) {
    # Note: miss[i] = 1 if not-missing and miss[i] = 2 if missing
    y[i] ~ dnorm(alpha[miss[i]] + beta * x[i], sigma^-2)
    m[i] ~ dbern(p[i])
    logit(p[i]) = gamma + delta * (x[i] - mean(x))
  }
  # Priors for regression model
  alpha[1] ~ dnorm(0, 100^-2)
  alpha[2] ~ dnorm(alpha[1], sigma_alpha^-2)
  beta ~ dnorm(0, 100^-2)
  sigma ~ dgamma(1, 1)
  sigma_alpha ~ dgamma(1, 1)
  # Priors for missingness model
  gamma ~ dnorm(0, 5^-2)
  delta ~ dnorm(0, 5^-2)
}
'

# Simulate some data for this situation
set.seed(123)
N = 100
x = sort(runif(N))
gamma = -0.5
delta = 1
m = rbinom(N, 1, plogis(gamma + delta * (x - mean(x)))) + 1
alpha = c(3,3.3)
beta = 2
sigma = 0.2
y_true = rnorm(N, alpha[m] + beta * x, sigma)
y = y_true
y[m==2] = NA

# Plot the simulations
plot(x, y_true)
points(x, y, col = 'red', pch = 19)

# Run the model
mod_par =  c("alpha", "beta", "sigma", "gamma", "delta", "y", "sigma_alpha")
model_run = jags(data = list(N = N, y = y, x = x, miss = m, m = m - 1),
                 parameters.to.save = mod_par,
                 model.file = textConnection(model_code))

# Check R-hat and parameters
plot(model_run)

# Again we have the truth here so can compare
pars = model_run$BUGSoutput$sims.list
par(mfrow=c(3,2))
hist(pars$alpha[,1], breaks = 30, main = 'Intercept (obs)'); abline(v = alpha[1], col='red')
hist(pars$alpha[,2], breaks = 30, main = 'Intercept (miss)'); abline(v = alpha[2], col='red')
hist(pars$beta, breaks = 30, main = 'Slope'); abline(v = beta, col='red')
hist(pars$gamma, breaks = 30, main = 'Miss intercept'); abline(v = gamma, col='red')
hist(pars$delta, breaks = 30, main = 'Miss coef of x'); abline(v = delta, col='red')
par(mfrow=c(1,1))
