# In this practical we will go through some analysis of missing data in the machine learning and related situations

# Packages we will use
library(mlr)
library(mlbench)
library(VarSelLCM)
library(mixtools)

# Quick intro: using mlr --------------------------------------------------

# Get the data
data(BostonHousing, package = "mlbench")

# Make a regression task (see also makeClassifTask)
bh_task = makeRegrTask(data = BostonHousing, target = "medv")

# Set up a learner
bh_learner = makeLearner("regr.lm")

# Train the model - (train function contained in many other packages)
bh_train = mlr::train(bh_learner, bh_task)

# Output the model
getLearnerModel(bh_train)

# Worth checking out https://mlr.mlr-org.com


# Missing data with mlr ---------------------------------------------------

# These are all the learners which allow for 'missings'
listLearners("regr", properties = "missings")[c("class", "package")]

# Use the airquality data
data(airquality)
str(airquality)

# This is mlr's impute function
imp = impute(airquality, classes = list(integer = imputeMean(), 
                                        factor = imputeMode()))
str(imp$data)

# If doing it properly, make sure to specify the target
imp = impute(airquality, target = "Ozone")

# Change the data slightly to allow a better imputation approach
airq = airquality
ind = sample(nrow(airq), 10)
airq$Wind[ind] = NA
airq$Wind = cut(airq$Wind, c(0,8,16,24))

# Can separate into train and test
airq = subset(airq, select = 1:4)
airq.train = airq[1:100,] # Take the first 100 rows
airq.test = airq[-c(1:100),]

# Fancier mlr imputation
imp = impute(airq.train, target = "Ozone", 
             cols = list(Solar.R = imputeHist(), 
                         Wind = imputeLearner("classif.rpart")), 
             dummy.cols = c("Solar.R", "Wind"))
summary(imp$data)

# And the imputation description
imp$desc

# We can now re-impute onto the test data:
airq.test.imp = reimpute(airq.test, imp$desc)
head(airq.test.imp)

# More here: https://mlr.mlr-org.com/articles/tutorial/impute.html

# Really you should start using the mlr3 package: https://mlr3.mlr-org.com

# Missing data with cluster analysis --------------------------------------

data(faithful)
faithful[1,1] = NA # Just to show it works with NAs

# You can play with gvals which might be more interesting than just two clusters, especially for variable selection
res = VarSelCluster(faithful, gvals = 2, vbleSelec = FALSE, crit.varsel = "BIC")

# Plot the output
plot(faithful)
points(res@param@paramContinuous@mu[1,], res@param@paramContinuous@mu[2,], col = 'red', pch = 19)
ellipse(mu=res@param@paramContinuous@mu[,1], sigma=diag(res@param@paramContinuous@sd[,1]), 
        alpha = .5, npoints = 250, col="red") 
ellipse(mu=res@param@paramContinuous@mu[,2], sigma=diag(res@param@paramContinuous@sd[,2]), 
        alpha = .5, npoints = 250, col="red") 

# The mclust package is (IMHO) better than VarSelLCM but it only has a basic imputation method

