# Some JAGS models for non-ignorable missingness

# Set up ------------------------------------------------------------------

library(R2jags)

# Simulate some data ------------------------------------------------------

# Let's simulate some data from the all the way back in class 1 and see if we can fit it using JAGS

n = 200
p = 2
X = mvrnorm(n, mu = rep(0,p), 
            Sigma = matrix(c(1, 0.5, 
                             0.5, 1), 
                           2, 2))
y = 3 + 2 * X[,1] - X[,2] + rnorm(n)

# Now simulate MAR and NMAR data and see if we can take account of it
m_mar  = rbinom(n, 1, plogis(X[, 1]))
m_nmar = rbinom(n, 1, plogis(X[, 2]))
x_mar = x_nmar = X
x_mar[which(m_mar == 1),2] = NA
x_nmar[which(m_nmar == 1),2] = NA

# MAR model (ignorable but includes ) -----------------------------------------------


