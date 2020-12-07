# An introduction to mice

# Intro to Rstudio --------------------------------------------------------

# A quick intro to Rstudio
# Panels: Console; Source, Environment; Files
# Good practice: write scripts
# Use projects
# Use templates
# <- vs =
# Code formatting and Ctrl/Cmd-I

# mice computational resource ---------------------------------------------

# The mice package can be found at:
# https://cran.r-project.org/web/packages/mice/index.html
# GitHub page is here:
# https://github.com/stefvanbuuren/mice
# A number of vignettes and code examples at the bottom of that page
# The whole mice book is here:
# https://stefvanbuuren.name/fimd/

# The mice package --------------------------------------------------------

library(mice)

# The data set we will use is nhanes:
help(nhanes)
head(nhanes)
str(nhanes)
summary(nhanes)
# 4 variables
# Age group, ordinal
# BMI
# Hypertensive (1 = no, 2 = yes)
# Cholesterol level
# We are interested in predicting cholesterol level from the other variables

# Show the missingness pattern
md.pattern(nhanes)
# Blue = observed, pink = missing

# Fit a first regression model
fit = lm(chl ~ ., data = nhanes)
summary(fit)
# No warnings but how many values did it fit to?
fit$fitted.values


# Single imputation -------------------------------------------------------

# NOTE: the mice command has a seed argument due to its use of random sampling, so if you want repeatable results please include it below, e.g. seed = 123

# Get the missing values filled in by basic mean imputation
imp = mice(nhanes, method = "mean", m = 1, maxit = 1)
str(imp)

# Access the complete data with 
complete(imp)
# Can check this with colMeans(nhanes, na.rm = TRUE)

# A better version is with regression imputation
imp2 = mice(nhanes, method = "norm.predict", m = 1, maxit = 1)
str(imp2)
complete(imp2)

# Could now run the regression on the completed data
fit2 = lm(bmi ~ ., data = complete(imp2))
summary(fit2)
# Look at the standard errors - all gone down

# Another version - this time use the stochastic regression approach
imp3 = mice(nhanes, method = "norm.nob", m = 1, maxit = 1)
fit3 = lm(bmi ~ ., data = complete(imp3))
summary(fit3)

# Multiple imputation -----------------------------------------------------

# The default mice arguments is for 5 imputations, over 5 iterations where the default methods are c("pmm", "logreg", "polyreg", "polr") corresponding to numeric data, binary factor data, multi-level factor data, and multi-level ordered factor data respectively

imp4 = mice(nhanes) # Can add m = 3 for different numbers of imputed data sets

# Show the different imputed data sets
imp4$imp
# The columns are the different imputed values for each missing observation

# You can export these data sets in different formats
complete(imp4, "long") 
# The original data set only had nrow(nhanes) = 25 observations
# This now as 5*25 = 125 rows
# You can also do it by binding the columns together with 
# complete(imp4, "broad")
# complete(imp4, "repeated") # Similar to above but each column repeated

# One thing you might have noticed is the predictor matrix
imp4
# This says which variables are used to impute the others
# If you want to change the predictor matrix you can extract it, change it, then give it back to mice
pred = imp4$predictorMatrix
# Suppose you don't want hypertension to help you impute the variables
# This might occur when you have known collinearity or many variables
pred[ ,"hyp"] <- 0
pred
imp5 = mice(nhanes, pred = pred, print = FALSE)
# There are some convenience functions for quickly selecting which predictors are used for the imputation (see ?quickpred)


# Finer algorithm details -------------------------------------------------

# The default plotting uses (slightly ugly) lattice plots
plot(imp5)
# The x-axis of the plot specify the iteration number (note this is the iteration not the number of imputations which is the different lines). 
# If it has 'worked' then the lines should jump around and not lie parallel
# Remember - no age as it's fully observed
# It's often worth increasing the number of iterations to get a better picture of convergence

# A strip plot can be obtained to see the behaviour of the imputed values
stripplot(imp5)
# Should show the missing data (red) against the observed (blue)

# The methods used to perform the imputation for each variable can be found via:
imp5$method
# These are all numeric variables so partial mean matching was used everywhere - this seems wrong...

# We can manually change the types of these variables, or we can just use the ...
str(nhanes2)
# ... data set

imp6 = mice(nhanes2, print = FALSE)
imp6$method

# Now look at the imputed values
complete(imp6)

# The full list of methods is given as:
methods(mice)

# We can extract the method from the last run and re-do it with a new method
meth_new = imp6$meth 
meth_new[2] = "norm" # ?mice.impute.norm - Bayesian LR

imp7 = mice(nhanes2, method = meth_new, print = FALSE)
imp7$method
plot(imp7)


# Running models with imputed data sets -----------------------------------

# You can fit a model using the imputed object from a mice run
fit4 = with(imp7, lm(chl ~ bmi + age + hyp)) # Annoyingly doesn't work with a data argument, or .
summary(fit4) # This has fitted the model to each data set in turn

# We can pick out individual fits, e.g.
summary(fit4$analyses[[2]])

# We can combine these fits together into one model
pool_fit = pool(fit4)
summary(pool_fit)

# pool_fit contains even more info
pool_fit
# lambda measures the proportion of total variance due to missingness the influence of the missing data on the model
# riv is the residual increase in variance due to to missingness
# fmi is the fraction of missing information
# If these values are high (fmi > 0.5?) then this indicates a poorly fitting imputation model. You might want to remove some of the variables from the imputation predictor matrix


# Other tasks -------------------------------------------------------------

# You could try some of the same analysis on the other data sets in the mice package
# See data(package = 'mice')
# And then pick your favourite
# 


