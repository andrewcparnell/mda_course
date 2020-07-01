# Using JAGS to impute missing data


# Set up ------------------------------------------------------------------

library(R2jags)

# Reminder of the nhanes data
str(nhanes)

# Introduction to JAGS ----------------------------------------------------

# A simple linear regression example

model_code = '
model
{
  # Likelihood
  for (i in 1:n) {
    chl[i] ~ dnorm(intercept + slope * bmi[i], residual_sd^-2)
  }

  # Priors
  intercept ~ dnorm(0, 100^-2)
  slope ~ dnorm(0, 100^-2)
  residual_sd ~ dunif(0, 100)
}
'

# Set up the data
nhanes_cc = nhanes[complete.cases(nhanes),]
with(nhanes_cc, plot(bmi, chl))

model_data = list(n = nrow(nhanes_cc), 
                  chl = nhanes_cc$chl, 
                  bmi = nhanes_cc$bmi)

# Choose the parameters to watch
model_parameters =  c("intercept", "slope", "residual_sd")

# Run the model
model_run = jags(data = model_data,
                 parameters.to.save = model_parameters,
                 model.file=textConnection(model_code))
print(model_run)
plot(model_run)


# Simple linear regression with missing values ----------------------------

# If you tried to run that on the full data it doesn't work
model_data2 = list(n = nrow(nhanes), 
                   chl = nhanes$chl, 
                   bmi = nhanes$bmi)
# jags(data = model_data2,
#      parameters.to.save = model_parameters,
#      model.file=textConnection(model_code))
# Interestingly this is because it is missing the bmi values, not because it is missing the chl values

# Instead change the model structure
model_code2 = '
model
{
  # Likelihood
  for (i in 1:n) {
    chl[i] ~ dnorm(intercept + slope * bmi[i], residual_sd^-2)
  }
  # Priors for the missing x values
  for(k in 1:n_miss_bmi) {
    bmi[miss[k]] ~ dunif(min_bmi, max_bmi)
  }
  
  # Priors
  intercept ~ dnorm(0, 100^-2)
  slope ~ dnorm(0, 100^-2)
  residual_sd ~ dunif(0, 100)
}
'

# First have to find which values of bmi are missing
miss = which(is.na(nhanes$bmi))
model_data2$miss = miss
model_data2$n_miss_bmi = length(miss)
model_data2$min_bmi = min(nhanes$bmi, na.rm = TRUE)
model_data2$max_bmi = max(nhanes$bmi, na.rm = TRUE)

model_run2 = jags(data = model_data2,
                  parameters.to.save = c(model_parameters, 'bmi'),
                  model.file=textConnection(model_code2))
print(model_run2)
plot(model_run2)
# Didn't work very well!
# This is largely because there is not a strong relationship between bmi and chl (the line is quite flat with large residual sd)
# We could put more information into this model by e.g. including covariates on the missing bmi


# A full imputation model -------------------------------------------------

model_code3 = '
model
{
  # Likelihood
  for (i in 1:n) {
    chl[i] ~ dnorm(fits[i], residual_sd^-2)
    fits[i] = intercept + 
                slope_bmi * bmi[i] + 
                slope_age[age[i]] + 
                slope_hyp[hyp[i]]
  }
  # Priors for the missing x values
  for(k in 1:n_miss_bmi) {
    bmi[miss_bmi[k]] ~ dunif(min_bmi, max_bmi)
  }
  for(k in 1:n_miss_hyp) {
    hyp[miss_hyp[k]] ~ dcat(p_hyp)
  }
  
  # Priors
  intercept ~ dnorm(0, 100^-2)
  slope_bmi ~ dnorm(0, 100^-2)
  for(l in 1:3) {
    slope_age[l] ~ dnorm(0, 100^-2)
  }
  for(l in 1:2) {
    slope_hyp[l] ~ dnorm(0, 100^-2)
  }
  residual_sd ~ dunif(0, 100)
}
'

miss_bmi = which(is.na(nhanes$bmi))
miss_hyp = which(is.na(nhanes$hyp))

model_data3 = list(n = nrow(nhanes), 
                   chl = nhanes$chl, 
                   bmi = nhanes$bmi,
                   age = nhanes$age,
                   hyp = nhanes$hyp,
                   miss_hyp = miss_hyp,
                   miss_bmi = miss_bmi,
                   n_miss_bmi = length(miss_bmi),
                   n_miss_hyp = length(miss_hyp),
                   min_bmi = min(nhanes$bmi, na.rm = TRUE),
                   max_bmi = max(nhanes$bmi, na.rm = TRUE),
                   p_hyp = c(0.5, 0.5))

model_run3 = jags(data = model_data3,
                  parameters.to.save = c('intercept', 'slope_bmi', 
                                         'slope_age', 'slope_hyp',
                                         'bmi', 'hyp', 'fits'),
                  model.file=textConnection(model_code3))
print(model_run3)
plot(model_run3)

# Plot the fitted values vs the true values
plot(nhanes$chl, model_run3$BUGSoutput$mean$fits)
axis(side = 2, at = model_run3$BUGSoutput$mean$fits,
     labels = FALSE)
abline(a = 0, b = 1, col = 'red')




