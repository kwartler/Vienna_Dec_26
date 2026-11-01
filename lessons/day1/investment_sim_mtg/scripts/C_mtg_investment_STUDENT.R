#' Ted Kwartler
#' Dec 11, 2026
#' Purpose: Is a box of Magic: The Gathering cards a good investment?
#'          Build small functions that open a pack, price it, open a box and
#'          simulate many boxes, then use the simulation to make a decision.
#'          Part 1 replays the original Guilds of Ravnica trade (2018).
#'          Part 2 asks the same question about Reality Fracture (2026).
#'          STUDENT version: each function has its name, inputs and steps.
#'          Write the body. The BASIC version and the KEY are there if you need them.
#' Data: TCGplayer market prices; pack odds from Wizards of the Coast

# libraries
library(triangle)
options(scipen = 999)

# Where the class data lives
dataPath <- 'https://raw.githubusercontent.com/kwartler/Vienna_Dec_26/refs/heads/main/lessons/day1/investment_sim_mtg/data/'

# ---------------------------------------------------------------------------
# 0. WARM UP - one card, one price
# ---------------------------------------------------------------------------

# Card prices are bounded and skewed: you rarely sell above market and often
# sell a bit below it. Draw one sale price from a triangle distribution.
cardPrice <- function(low, market) {
  # if market is not above low, the price is just market
  # otherwise draw 1 value with rtriangle(): min low, max market, mode market
  # return the price
}
set.seed(1234)
cardPrice(low = 3, market = 4.99)
cardPrice(low = 3, market = 4.99)   # run it again; what changes?

# ---------------------------------------------------------------------------
# 1. GUILDS OF RAVNICA (2018): 36 packs per box, $79.50 per box
# ---------------------------------------------------------------------------
guilds <- read.csv(paste0(dataPath, 'guilds_of_ravnica_10_16_18.csv'))
head(guilds)
table(guilds$rarity)

# Open one classic 15 card booster pack
crackPack <- function(cardDF, packsPerMythic = 8, packsPerFoil = 6) {
  # 1. subset cardDF into onlyCommon, onlyUncommon, onlyRare, onlyMythic (codes C, U, R, M)
  # 2. sample 10 commons and 3 uncommons (sample row numbers, then index the rows)
  # 3. a weighted coin flip: TRUE with probability 1 / packsPerMythic
  #    TRUE gives one mythic, else one rare
  # 4. a second weighted coin flip with 1 / packsPerFoil
  #    TRUE gives one card from all four rarities (the foil), else one more common
  # 5. rbind the 15 cards, add a logical foil column (only the last card can be TRUE)
  # 6. return the pack
}
onePack <- crackPack(guilds)
onePack

# What can we actually sell the pack for?
# Cards under $1 are bulk (dealers pay about $5 per 1,000 cards) and the
# marketplace keeps about 15% of every sale.
packValue <- function(pack, sellFee = 0.15, bulkCutoff = 1, bulkPrice = 0.005) {
  # 1. loop over the rows of pack and draw a cardPrice() for each card
  # 2. cards at or above bulkCutoff earn price * (1 - sellFee); the rest earn bulkPrice
  # 3. return the sum
}
packValue(onePack)
packValue(onePack, sellFee = 0, bulkCutoff = 0)   # at market, no frictions

# Open a whole box. packFun is the function that opens one pack, so the same
# openBox() works for any set. Pass ... through to packFun.
openBox <- function(cardDF, packFun, nPacks = 36, ...) {
  # 1. make an empty numeric vector of length nPacks
  # 2. loop nPacks times: open a pack with packFun(cardDF, ...) and store its packValue()
  # 3. return the vector of pack values
}
oneBox <- openBox(guilds, crackPack, nPacks = 36)
sum(oneBox)
sum(oneBox) - 79.50   # profit or loss on this one box

# One box is one draw. Simulate many boxes to see the range of outcomes.
simulateBoxes <- function(cardDF, packFun, nBoxes = 100, nPacks = 36, boxCost = 79.50, ...) {
  # 1. loop nBoxes times; each revenue is the sum of openBox(cardDF, packFun, nPacks, ...)
  # 2. return a data frame with boxID, revenue and profit (revenue - boxCost)
}
set.seed(2018)
guildsSim <- simulateBoxes(guilds, crackPack, nBoxes = 100, nPacks = 36, boxCost = 79.50)
summary(guildsSim$revenue)

plot(density(guildsSim$revenue), main = 'Guilds of Ravnica: revenue per box',
     xlab = 'Net revenue per box (USD)')
abline(v = 79.50, col = 'red')
abline(v = mean(guildsSim$revenue), col = 'blue')
mean(guildsSim$profit < 0)

# ---------------------------------------------------------------------------
# 2. DECISION FUNCTIONS
# ---------------------------------------------------------------------------

# The most you should pay per box to make money in `confidence` share of boxes
maxBid <- function(revenue, confidence = 0.8) {
  # hint: quantile() at probs = 1 - confidence
}
maxBid(guildsSim$revenue)
maxBid(guildsSim$revenue, confidence = 0.5)

# Compare opening the box with not opening it
compareStrategies <- function(sims, boxCost, sealedPrice, packPrice, nPacks, sellFee = 0.15) {
  # return a data frame with one row per strategy:
  #   open and sell singles, sell the box sealed, sell the packs one at a time
  # columns: strategy, expectedProfit, chanceOfLoss (all net of sellFee)
}

# ---------------------------------------------------------------------------
# 3. REALITY FRACTURE (2026): 30 Play Boosters per box
# ---------------------------------------------------------------------------
rf     <- read.csv(paste0(dataPath, 'reality_fracture_play_booster.csv'))
odds   <- read.csv(paste0(dataPath, 'play_booster_collation.csv'))
sealed <- read.csv(paste0(dataPath, 'reality_fracture_sealed_prices.csv'))
head(rf)
odds

# This price list only has a market price. Add a low price so cardPrice() works.
addLowPrice <- function(cardDF, discount = 0.25) {
  # add a low column: market reduced by discount, rounded to cents; return cardDF
}
rf <- addLowPrice(rf)

# Wizards publishes some odds only as "<1%"; use half a percent
odds$probability[is.na(odds$probability)] <- 0.005

# A Play Booster is built slot by slot. Each slot has its own odds table.
# These two are complete. Read them and explain each line to a partner.
openSlot <- function(cardDF, slotOdds) {
  pick    <- sample(1:nrow(slotOdds), 1, prob = slotOdds$probability)
  outcome <- slotOdds[pick, ]
  pool    <- subset(cardDF, cardDF$rarity == outcome$rarity &
                            cardDF$treatment == outcome$treatment &
                            cardDF$echoed == (outcome$slot == 'echoed_pair'))
  nCards  <- outcome$cards_in_slot
  cards   <- pool[sample(1:nrow(pool), nCards, replace = nrow(pool) < nCards), ]
  cards$foil <- rep(outcome$foil == 'yes', nCards)
  cards$slot <- rep(outcome$slot, nCards)
  return(cards)
}

openPlayBooster <- function(cardDF, odds) {
  slots <- unique(odds$slot)
  pack  <- list()
  for (i in 1:length(slots)) {
    slotOdds  <- subset(odds, odds$slot == slots[i])
    pack[[i]] <- openSlot(cardDF, slotOdds)
  }
  pack <- do.call(rbind, pack)
  return(pack)
}
set.seed(2026)
rfPack <- openPlayBooster(rf, odds)
rfPack[, c('name', 'rarity', 'treatment', 'foil', 'market')]
packValue(rfPack)

boxCost     <- 100      # estimated distributor price per box, Nov 2026
sealedPrice <- sealed$market_price_usd[sealed$product == 'Reality Fracture - Play Booster Display']
packPrice   <- sealed$market_price_usd[sealed$product == 'Reality Fracture - Play Booster']

# Simulate 100 Reality Fracture boxes with your functions (30 packs, pass odds = odds)
rfSim <- ______

# Plot the revenue distribution with the cost and the mean, as in Part 1

# ---------------------------------------------------------------------------
# 4. THEN VS NOW - the decision
# ---------------------------------------------------------------------------
# 2018: assume a sealed box resold near what we paid and packs sold at $3.99 retail
compareStrategies(guildsSim, boxCost = 79.50, sealedPrice = 79.50,
                  packPrice = 3.99, nPacks = 36)
compareStrategies(rfSim, boxCost = boxCost, sealedPrice = sealedPrice,
                  packPrice = packPrice, nPacks = 30)

# ---------------------------------------------------------------------------
# 5. EXTEND
# ---------------------------------------------------------------------------
# Which assumption moves the answer the most: the sell fee, the bulk price,
# or the discount in addLowPrice()? Loop over a few values of one of them
# and record the mean revenue per box. What would you do with 100 boxes?

# End
