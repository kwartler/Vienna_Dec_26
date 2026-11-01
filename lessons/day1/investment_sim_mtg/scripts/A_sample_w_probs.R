#' Ted Kwartler
#' Dec 11, 2026
#' Purpose: See sample() with probability weights

idx <- 1:10

# Equal Probs
set.seed(123)
sample(idx,3)

# Example Probs
set.seed(123)
sample(idx, 3, replace = T, prob = c(1,0,0,0,0,0,0,0,0,0))


# Example Probs 2
set.seed(123)
sample(idx, 3, replace = T, prob = c(.5,.5,0,0,0,0,0,0,0,0))


# Example Probs 2
set.seed(123)
sample(idx, 3, replace = T, prob = c(.33,.33,0,0,0,0,.33,0,0,0))

set.seed(1234)
sample(idx, 3, replace = T, prob = c(.33,.33,0,0,0,0,.33,0,0,0))

# Magic example: the rare slot gives a mythic about 1 pack in 8
set.seed(123)
sample(c('rare', 'mythic'), 8, replace = T, prob = c(7/8, 1/8))

# Over many packs the share of mythics settles near 1/8
set.seed(123)
slot <- sample(c('rare', 'mythic'), 10000, replace = T, prob = c(7/8, 1/8))
table(slot) / length(slot)

# End
