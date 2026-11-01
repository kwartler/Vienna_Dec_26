#' Ted Kwartler
#' Dec 11, 2026
#' Purpose: Is a box of Magic: The Gathering cards a good investment?
#'          Build small functions that open a pack, price it, open a box and
#'          simulate many boxes, then use the simulation to make a decision.
#'          Part 1 replays the original Guilds of Ravnica trade (2018).
#'          Part 2 asks the same question about Reality Fracture (2026).
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
  if (market <= low) {
    price <- market
  } else {
    price <- rtriangle(1, a = low, b = market, c = market)
  }
  return(price)
}
set.seed(1234)
cardPrice(low = 3, market = 4.99)
cardPrice(low = 3, market = 4.99)   # run it again; it changes

# ---------------------------------------------------------------------------
# 1. GUILDS OF RAVNICA (2018): 36 packs per box, $79.50 per box
# ---------------------------------------------------------------------------
guilds <- read.csv(paste0(dataPath, 'guilds_of_ravnica_10_16_18.csv'))
head(guilds)
table(guilds$rarity)

# Open one classic 15 card booster pack
# 10 commons, 3 uncommons, 1 rare (a mythic 1 in 8 packs),
# and a last card that is a foil 1 in 6 packs, otherwise another common
crackPack <- function(cardDF, packsPerMythic = 8, packsPerFoil = 6) {
  onlyCommon   <- subset(cardDF, cardDF$rarity == 'C')
  onlyUncommon <- subset(cardDF, cardDF$rarity == 'U')
  onlyRare     <- subset(cardDF, cardDF$rarity == 'R')
  onlyMythic   <- subset(cardDF, cardDF$rarity == 'M')

  commons   <- onlyCommon[sample(1:nrow(onlyCommon), 10), ]
  uncommons <- onlyUncommon[sample(1:nrow(onlyUncommon), 3), ]

  mythicCheck <- sample(c(TRUE, FALSE), 1,
                        prob = c(1 / packsPerMythic, 1 - 1 / packsPerMythic))
  if (mythicCheck == TRUE) {
    premium <- onlyMythic[sample(1:nrow(onlyMythic), 1), ]
  } else {
    premium <- onlyRare[sample(1:nrow(onlyRare), 1), ]
  }

  foilCheck <- sample(c(TRUE, FALSE), 1,
                      prob = c(1 / packsPerFoil, 1 - 1 / packsPerFoil))
  if (foilCheck == TRUE) {
    foilPool <- rbind(onlyCommon, onlyUncommon, onlyRare, onlyMythic)
    lastCard <- foilPool[sample(1:nrow(foilPool), 1), ]
  } else {
    lastCard <- onlyCommon[sample(1:nrow(onlyCommon), 1), ]
  }

  pack      <- rbind(commons, uncommons, premium, lastCard)
  pack$foil <- c(rep(FALSE, 14), foilCheck)
  return(pack)
}
onePack <- crackPack(guilds)
onePack

# What can we actually sell the pack for?
# Cards under $1 are bulk (dealers pay about $5 per 1,000 cards) and the
# marketplace keeps about 15% of every sale.
packValue <- function(pack, sellFee = 0.15, bulkCutoff = 1, bulkPrice = 0.005) {
  values <- numeric(nrow(pack))
  for (i in 1:nrow(pack)) {
    values[i] <- cardPrice(pack$low[i], pack$market[i])
  }
  netValues <- ifelse(values >= bulkCutoff, values * (1 - sellFee), bulkPrice)
  return(sum(netValues))
}
packValue(onePack)
packValue(onePack, sellFee = 0, bulkCutoff = 0)   # at market, no frictions

# Open a whole box: packFun is the function that opens one pack.
# Passing a function into a function lets openBox() work for any set.
openBox <- function(cardDF, packFun, nPacks = 36, ...) {
  packValues <- numeric(nPacks)
  for (i in 1:nPacks) {
    onePack       <- packFun(cardDF, ...)
    packValues[i] <- packValue(onePack)
  }
  return(packValues)
}
oneBox <- openBox(guilds, crackPack, nPacks = 36)
oneBox
sum(oneBox)
sum(oneBox) - 79.50   # profit or loss on this one box

# One box is one draw. Simulate many boxes to see the range of outcomes.
simulateBoxes <- function(cardDF, packFun, nBoxes = 100, nPacks = 36, boxCost = 79.50, ...) {
  revenue <- numeric(nBoxes)
  for (j in 1:nBoxes) {
    revenue[j] <- sum(openBox(cardDF, packFun, nPacks, ...))
    cat('.')
  }
  out <- data.frame(boxID   = 1:nBoxes,
                    revenue = revenue,
                    profit  = revenue - boxCost)
  return(out)
}
set.seed(2018)
guildsSim <- simulateBoxes(guilds, crackPack, nBoxes = 100, nPacks = 36, boxCost = 79.50)
summary(guildsSim$revenue)

plot(density(guildsSim$revenue), main = 'Guilds of Ravnica: revenue per box',
     xlab = 'Net revenue per box (USD)')
abline(v = 79.50, col = 'red')
abline(v = mean(guildsSim$revenue), col = 'blue')

# Share of boxes that lost money
mean(guildsSim$profit < 0)

# ---------------------------------------------------------------------------
# 2. DECISION FUNCTIONS
# ---------------------------------------------------------------------------

# The most you should pay per box to make money in `confidence` share of boxes
maxBid <- function(revenue, confidence = 0.8) {
  bid <- quantile(revenue, probs = 1 - confidence, names = FALSE)
  return(bid)
}
maxBid(guildsSim$revenue)
maxBid(guildsSim$revenue, confidence = 0.5)

# Compare opening the box with not opening it
compareStrategies <- function(sims, boxCost, sealedPrice, packPrice, nPacks, sellFee = 0.15) {
  sealedNet <- sealedPrice * (1 - sellFee)
  looseNet  <- nPacks * packPrice * (1 - sellFee)
  out <- data.frame(strategy       = c('Open and sell singles',
                                       'Sell the box sealed',
                                       'Sell the packs one at a time'),
                    expectedProfit = round(c(mean(sims$revenue) - boxCost,
                                             sealedNet - boxCost,
                                             looseNet - boxCost), 2),
                    chanceOfLoss   = c(mean(sims$revenue < boxCost),
                                       as.numeric(sealedNet < boxCost),
                                       as.numeric(looseNet < boxCost)))
  return(out)
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
  cardDF$low <- round(cardDF$market * (1 - discount), 2)
  return(cardDF)
}
rf <- addLowPrice(rf)

# Wizards publishes some odds only as "<1%"; use half a percent
odds$probability[is.na(odds$probability)] <- 0.005

# A Play Booster is built slot by slot. Each slot has its own odds table.
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
openSlot(rf, subset(odds, odds$slot == 'rare_mythic'))

# Loop over every slot to build one 15 card pack
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
sum(rfPack$market)
packValue(rfPack)

# The same openBox() and simulateBoxes() work: just pass a different pack function
boxCost     <- 100      # estimated distributor price per box, Nov 2026
sealedPrice <- sealed$market_price_usd[sealed$product == 'Reality Fracture - Play Booster Display']
packPrice   <- sealed$market_price_usd[sealed$product == 'Reality Fracture - Play Booster']

set.seed(2026)
rfSim <- simulateBoxes(rf, openPlayBooster, nBoxes = 100, nPacks = 30,
                       boxCost = boxCost, odds = odds)
summary(rfSim$revenue)

plot(density(rfSim$revenue), main = 'Reality Fracture: revenue per box',
     xlab = 'Net revenue per box (USD)', xlim = c(0, max(boxCost, rfSim$revenue) * 1.1))
abline(v = boxCost, col = 'red')
abline(v = mean(rfSim$revenue), col = 'blue')

mean(rfSim$profit < 0)
maxBid(rfSim$revenue)

# ---------------------------------------------------------------------------
# 4. THEN VS NOW - the decision
# ---------------------------------------------------------------------------
# 2018: assume a sealed box resold near what we paid and packs sold at $3.99 retail
compareStrategies(guildsSim, boxCost = 79.50, sealedPrice = 79.50,
                  packPrice = 3.99, nPacks = 36)
compareStrategies(rfSim, boxCost = boxCost, sealedPrice = sealedPrice,
                  packPrice = packPrice, nPacks = 30)

# Cost per pack then and now
79.50 / 36
boxCost / 30

# 100 boxes of Reality Fracture, opened and sold as singles
100 * mean(rfSim$profit)

# ---------------------------------------------------------------------------
# 5. EXTEND - because each step is a function, new questions are cheap
# ---------------------------------------------------------------------------
# What if the marketplace fee drops to 10% and bulk is worth nothing at all?
# Write a version of packValue() with new defaults, or pass new arguments
# through. Which assumption moves the answer the most: fee, bulk price, or
# the low price discount in addLowPrice()?
sensitivity <- data.frame(discount = c(0.10, 0.25, 0.40), meanRevenue = NA)
for (i in 1:nrow(sensitivity)) {
  rfTest <- addLowPrice(rf, discount = sensitivity$discount[i])
  set.seed(1)
  sensitivity$meanRevenue[i] <- mean(simulateBoxes(rfTest, openPlayBooster, nBoxes = 20,
                                                   nPacks = 30, boxCost = boxCost,
                                                   odds = odds)$revenue)
}
sensitivity

# End
