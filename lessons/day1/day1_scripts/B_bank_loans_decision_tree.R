#' Author: Ted Kwartler
#' Date: Dec 11, 2026
#' Purpose: Load data and build a decision tree
#' https://archive.ics.uci.edu/ml/datasets/bank+marketing

# Options
options(scipen=999)

# Load the libraries
library(caret)
library(rpart.plot) #visualizing

# Bring in some data
dat <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/bank2.csv')

# Partitioning
splitPercent <- round(nrow(dat) * .9)
totalRecords <- 1:nrow(dat)
set.seed(1234)
idx <- sample(totalRecords, splitPercent)

trainDat <- dat[idx,]
testDat  <- dat[-idx,]

## EDA
summary(trainDat)
head(trainDat)
table(trainDat$y)

# Force a full tree (override default parameters)
# Don't do this one in your case!
overFit <- rpart(as.factor(y) ~ ., 
                 data = trainDat, 
                 method = "class", 
                 minsplit = 1, 
                 minbucket = 1, 
                 cp=-1)

# Look at all the rules!!
overFit

# Plotting the full tree takes a while, so skip it in class.
#prp(overFit, extra = 1)

# Look at training probabilities
trainProbs <- predict(overFit, trainDat) 
head(trainProbs, 10)

# Get the final class and actuals
trainClass <- data.frame(class = colnames(trainProbs)[max.col(trainProbs)],
                         actual = trainDat$y)
head(trainClass, 10)

# Confusion Matrix
confMat <- table(trainClass$class,trainClass$actual)
confMat

# Accuracy
sum(diag(confMat))/sum(confMat)

# Now predict on the test set
testProbs <- predict(overFit, testDat)

# Get the final class and actuals
testClass<-data.frame(class  = colnames(testProbs)[max.col(testProbs)],
                      actual = testDat$y)

# Confusion Matrix
confMat <- table(testClass$class,testClass$actual)
confMat

# Accuracy
sum(diag(confMat))/sum(confMat)

# Start over
rm(list=ls())

##########
dat <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/bank-full_v2.csv') # now a bit more data to approximate a real scenario


# To save time in class, we are only training on 50% of the data
splitPercent <- round(nrow(dat) * .5)
totalRecords <- 1:nrow(dat)
set.seed(1234)
idx <- sample(totalRecords, splitPercent)

trainDat <- dat[idx,]
testDat  <- dat[-idx,]


## EDA
summary(trainDat)
head(trainDat)

# No modification needed in this cleaned up data set. One could engineer some interactions though.

# Fit a decision tree with caret
set.seed(1234)
fit <- train(as.factor(y) ~., #formula based
             data = trainDat, #data in
             # caret's rpart method does recursive partitioning (trees)
             method = "rpart", 
             # Define a range for the cp to test
             tuneGrid = data.frame(cp = c(0.0001, 0.001,0.01, 0.05, 0.07)), 
             # i.e. a node needs at least 1 record to split and every terminal node keeps at least 2 records
             control = rpart.control(minsplit = 1, minbucket = 2)) 

# Or without the comments:
#fit <- train(y ~., data = trainDat, 
#             method = "rpart", 
#             tuneGrid = data.frame(cp = c(0.01, 0.05)), 
#             control = rpart.control(minsplit = 1, minbucket = 2)) 

# Examine
fit

# Plot the cp and accuracy relationship to adjust the tuneGrid inputs
plot(fit)

# Plot a pruned tree
if (!is.null(dev.list())) {
  dev.off()
} else {
  NULL
}
prp(fit$finalModel, extra = 1)

# Make some predictions on the training set
trainCaret <- predict(fit, trainDat)
head(trainCaret)

# Get the confusion matrix
confusionMatrix(trainCaret, as.factor(trainDat$y))

# Now more consistent accuracy & fewer rules!
testCaret <- predict(fit,testDat)
confusionMatrix(testCaret,as.factor(testDat$y))

# And if you need a probability outcome
testCaretProbs <- predict(fit,testDat, type = 'prob')
head(testCaretProbs) # because these are specific cut points and it's the average in the terminal node, you see repeating probability values

# End
