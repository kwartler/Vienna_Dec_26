#' Ted Kwartler
#' Dec 11, 2026
#' Purpose: Refactor the INN Hotels cancellation workflow into reusable functions.
#'          The morning challenge repeated the same scoring steps for every model
#'          and every data partition; here each repeated task becomes one function.
#' Source: https://www.kaggle.com/datasets/mariyamalshatta/inn-hotels-group

# library & options
library(ggplot2)
library(rpart)
library(randomForest)
library(MLmetrics)
library(vtreat)
options(scipen = 999)

# ---------------------------------------------------------------------------
# 0. WARM UP - three small functions
# ---------------------------------------------------------------------------

# Format a proportion as a percent string
toPct <- function(x, digits = 1) {
  out <- paste0(round(x * 100, digits), '%')
  return(out)
}
toPct(0.3274)
toPct(0.3274, digits = 0)

# Share of bookings in a data frame that were canceled
cancelRate <- function(df) {
  rate <- mean(df$booking_status == 'Canceled')
  return(rate)
}

# Label one room price; if() handles one value at a time
priceBand <- function(price) {
  if (price < 80) {
    band <- 'budget'
  } else if (price < 150) {
    band <- 'standard'
  } else {
    band <- 'premium'
  }
  return(band)
}
priceBand(95)
sapply(c(60, 95, 210), priceBand)

# ---------------------------------------------------------------------------
# 1. FUNCTIONS
# ---------------------------------------------------------------------------

# Split a data frame into a vtreat design set, a training set and a validation set
splitData <- function(df, treatPct = 0.1, trainPct = 0.8, seed = 1234) {
  set.seed(seed)
  treatIdx  <- sample(1:nrow(df), nrow(df) * treatPct)
  treatRows <- df[treatIdx, ]
  remaining <- df[-treatIdx, ]
  trainIdx  <- sample(1:nrow(remaining), nrow(remaining) * trainPct)
  out <- list(treat      = treatRows,
              training   = remaining[trainIdx, ],
              validation = remaining[-trainIdx, ])
  return(out)
}

# Apply a vtreat plan, recode the outcome to 1/0 and drop the engineered _cat variables
prepData <- function(plan, df, yName = 'booking_status', successLevel = 'Not_Canceled') {
  treated <- prepare(plan, df)
  treated[[yName]] <- ifelse(treated[[yName]] == successLevel, 1, 0)
  treated <- treated[, !grepl('_cat', names(treated))]
  return(treated)
}

# Return the probability of the success class (1) for any of our three model types
getProbs <- function(model, newData) {
  if (inherits(model, 'glm')) {
    probs <- predict(model, newData, type = 'response')
  } else if (inherits(model, 'randomForest')) {
    probs <- predict(model, newData, type = 'prob')[, '1']
  } else if (inherits(model, 'rpart')) {
    probs <- predict(model, newData)[, '1']
  } else {
    stop('getProbs() does not know how to score a model of class ', class(model)[1])
  }
  return(as.numeric(probs))
}

# Turn probabilities into classes at a cutoff and report the confusion matrix and accuracy
scoreModel <- function(actual, probs, cutoff = 0.5) {
  if (length(actual) != length(probs)) {
    stop('actual and probs must be the same length')
  } else {
    predicted <- ifelse(probs >= cutoff, 1, 0)
  }
  confusion <- table(actual = actual, predicted = predicted)
  acc       <- Accuracy(y_pred = predicted, y_true = actual)
  return(list(confusion = confusion, accuracy = acc))
}

# ---------------------------------------------------------------------------
# 2. SAMPLE
# ---------------------------------------------------------------------------
hotelData <- read.csv('https://raw.githubusercontent.com/kwartler/teaching-datasets/refs/heads/main/INNHotelsGroup.csv')
parts     <- splitData(hotelData)
sapply(parts, nrow)

# The warm up functions work on any data frame with a booking_status column
toPct(cancelRate(hotelData))
toPct(cancelRate(parts$training))
table(sapply(hotelData$avg_price_per_room, priceBand))

# ---------------------------------------------------------------------------
# 3. EXPLORE (same as the morning, now on parts$training)
# ---------------------------------------------------------------------------
head(parts$training)
aggregate(avg_price_per_room ~ booking_status, parts$training, mean)

# ---------------------------------------------------------------------------
# 4. MODIFY
# ---------------------------------------------------------------------------
plan <- designTreatmentsC(dframe        = parts$treat,
                          varlist       = names(hotelData)[2:18],
                          outcomename   = 'booking_status',
                          outcometarget = 'Not_Canceled')
treatedTrain      <- prepData(plan, parts$training)
treatedValidation <- prepData(plan, parts$validation)

# ---------------------------------------------------------------------------
# 5. MODEL - three models kept together in a named list
# ---------------------------------------------------------------------------
models <- list(
  logistic = glm(booking_status ~ ., treatedTrain, family = 'binomial'),
  rpart    = rpart(as.factor(booking_status) ~ ., treatedTrain,
                   control = rpart.control(cp = 0.05)),
  rf       = randomForest(as.factor(booking_status) ~ ., treatedTrain, ntree = 250)
)

# ---------------------------------------------------------------------------
# 6. ASSESS - one loop replaces the eighteen repeated lines from the morning
# ---------------------------------------------------------------------------
results <- data.frame()
for (i in 1:length(models)) {
  trainScore <- scoreModel(treatedTrain$booking_status,
                           getProbs(models[[i]], treatedTrain))
  validScore <- scoreModel(treatedValidation$booking_status,
                           getProbs(models[[i]], treatedValidation))
  oneRow <- data.frame(model          = names(models)[i],
                       trainAccuracy  = round(trainScore$accuracy, 4),
                       validAccuracy  = round(validScore$accuracy, 4))
  results <- rbind(results, oneRow)
}
results

# The confusion matrix for any single model is one call away
scoreModel(treatedValidation$booking_status,
           getProbs(models$rf, treatedValidation))$confusion

# Same idea with lapply: score every model on validation at a stricter cutoff
validAt60 <- lapply(models, function(m) {
  s <- scoreModel(treatedValidation$booking_status, getProbs(m, treatedValidation), cutoff = 0.6)
  return(s$accuracy)
})
unlist(validAt60)

# ---------------------------------------------------------------------------
# 7. EXTEND - because the steps are functions, new questions are cheap
# ---------------------------------------------------------------------------
# Which cutoff gives the random forest its best validation accuracy?
cutoffs  <- seq(0.3, 0.8, by = 0.05)
rfProbs  <- getProbs(models$rf, treatedValidation)
cutAcc   <- numeric(length(cutoffs))
for (i in 1:length(cutoffs)) {
  cutAcc[i] <- scoreModel(treatedValidation$booking_status, rfProbs, cutoff = cutoffs[i])$accuracy
}
data.frame(cutoff = cutoffs, accuracy = round(cutAcc, 4))

# Challenge answer: return the cutoff with the highest accuracy
bestCutoff <- function(actual, probs, cutoffs = seq(0.3, 0.8, by = 0.05)) {
  acc <- numeric(length(cutoffs))
  for (i in 1:length(cutoffs)) {
    acc[i] <- scoreModel(actual, probs, cutoff = cutoffs[i])$accuracy
  }
  return(cutoffs[which.max(acc)])
}
sapply(models, function(m) bestCutoff(treatedValidation$booking_status, getProbs(m, treatedValidation)))

# End
