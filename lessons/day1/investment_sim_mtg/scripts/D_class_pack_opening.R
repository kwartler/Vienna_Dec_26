#' Ted Kwartler
#' Dec 11, 2026
#' Purpose: Open the real Reality Fracture box in class and compare it to the simulation.
#'          Before opening: everyone writes down a guess for their pack's market value.
#'          While opening: enter each card's collector number (bottom left of the card)
#'          into the class pulls sheet. Tokens are entered as T.
#'          After: how close did the simulation get?

# libraries
library(triangle)
options(scipen = 999)

# Functions from the lab
source('https://raw.githubusercontent.com/kwartler/Vienna_Dec_26/refs/heads/main/lessons/day1/investment_sim_mtg/scripts/Z_mtg_functions.R')

dataPath <- 'https://raw.githubusercontent.com/kwartler/Vienna_Dec_26/refs/heads/main/lessons/day1/investment_sim_mtg/data/'
rf   <- read.csv(paste0(dataPath, 'reality_fracture_play_booster.csv'))
odds <- read.csv(paste0(dataPath, 'play_booster_collation.csv'))
odds$probability[is.na(odds$probability)] <- 0.005

# ---------------------------------------------------------------------------
# 1. BEFORE OPENING - what does the simulation expect from one pack?
# ---------------------------------------------------------------------------
# Market value, no fees: the number we will check against the real pulls
set.seed(1211)
simPacks <- numeric(1000)
for (i in 1:length(simPacks)) {
  simPacks[i] <- sum(openPlayBooster(rf, odds)$market)
}
mean(simPacks)                              # the "sum average" per pack
quantile(simPacks, c(0.1, 0.5, 0.9))        # most packs land in this range
mean(simPacks > 5.23)                       # share of packs worth more than a pack costs

# A full 30 pack box at market value
set.seed(1211)
simBoxes <- numeric(100)
for (j in 1:length(simBoxes)) {
  simBoxes[j] <- sum(sapply(1:30, function(k) sum(openPlayBooster(rf, odds)$market)))
}
mean(simBoxes)

# ---------------------------------------------------------------------------
# 2. AFTER OPENING - value the real pulls
# ---------------------------------------------------------------------------
# Live class file: copy class_pulls_template.csv, fill it in, and read it here.
# class_pulls_example.csv is a simulated box so the script runs before class.
pulls       <- read.csv(paste0(dataPath, 'class_pulls_example.csv'), colClasses = 'character')
predictions <- read.csv(paste0(dataPath, 'class_predictions_example.csv'))

# Look up the market price of each card by collector number
# Tokens (T) get the median token price; anything not found is NA
lookupPrice <- function(numbers, cardDF) {
  cards      <- subset(cardDF, cardDF$treatment != 'token')
  tokenPrice <- median(cardDF$market[cardDF$treatment == 'token'])
  prices     <- cards$market[match(numbers, cards$number)]
  prices[numbers == 'T'] <- tokenPrice
  return(prices)
}
pulls$market <- lookupPrice(pulls$collector_number, rf)
subset(pulls, is.na(pulls$market))     # check typos or Special Guests cards

# Each student's pack
packTotals <- aggregate(market ~ student, pulls, sum)
packTotals <- merge(packTotals, predictions, by = 'student')
packTotals$guessError <- abs(packTotals$prediction - packTotals$market)
packTotals[order(packTotals$market, decreasing = TRUE), ]

# Prizes: best pack and closest guess
packTotals[which.max(packTotals$market), ]
packTotals[which.min(packTotals$guessError), ]

# The best pulls in the room
pulls[order(pulls$market, decreasing = TRUE), ][1:10, ]

# ---------------------------------------------------------------------------
# 3. HOW CLOSE DID WE GET?
# ---------------------------------------------------------------------------
c(simulatedPerPack = mean(simPacks), classPerPack = mean(packTotals$market))
c(simulatedBox = mean(simBoxes), ourBox = sum(packTotals$market))

# Where does our box fall among the simulated boxes?
mean(simBoxes <= sum(packTotals$market))

hist(simPacks, breaks = 40, main = 'Simulated packs vs our packs',
     xlab = 'Market value per pack (USD)', col = 'grey85', border = 'white')
rug(packTotals$market, col = 'red', lwd = 2)
abline(v = mean(simPacks), col = 'blue')
abline(v = mean(packTotals$market), col = 'red')

# Discussion: one box is one draw. Would you have predicted our box from the
# simulation? What would it take to trust the simulation with 100 boxes?

# End
