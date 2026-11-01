#' Ted Kwartler
#' Dec 11, 2026
#' Purpose: Create an example of the triangle distribution

# Install and load triangle library
library(triangle)

# Define distribution parameters
a <- 1  # Minimum
b <- 10 # Maximum
c <- 5  # Mode

# Create triangle distribution object
dist <- rtriangle(10000,a, b, c)

# Quick plot
plot(hist(dist))

# Define distribution parameters
a <- 1  # Minimum
b <- 10 # Maximum
c <- 3  # Mode

# Create triangle distribution object
dist <- rtriangle(10000,a, b, c)

# Quick plot
plot(hist(dist))

# A card: lowest listing $3.00, market $4.99. Most sales land near market.
cardSales <- rtriangle(10000, a = 3, b = 4.99, c = 4.99)
hist(cardSales, main = 'Simulated sale prices for one card')
mean(cardSales)

# End
