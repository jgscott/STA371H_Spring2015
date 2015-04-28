library(mosaic)
library(fImport)

# Import some ETF's corresponding to broad asset classes
# stocks, govt bonds, corporate bonds, commodities, real estate
mytickers = c("SPY", "TLT", "LQD", "DBC", "VNQ")
myprices = yahooSeries(mytickers, from='2008-01-01', to='2013-12-31')

# Compute the returns from the closing prices
# Remember to copy/paste (or command-enter) the computereturns function from previous script.
# Otherwise R won't know what the 'computereturns' function is
myreturns = computereturns(myprices)
head(myreturns)

# Pairwise scatterplots and a correlation matrix
pairs(myreturns)
cor(myreturns)

# Daily mean return in percentage points
100*colMeans(myreturns)

# Daily mean return in basis points
10000*colMeans(myreturns)

# Show the actual histogram of returns
hist(myreturns[,1], 50)


# Compare with the best-fitting normal approximation
hist(myreturns[,1], 50, prob=TRUE)
curve(dnorm(x, mean(myreturns[,1]), sd(myreturns[,1])), add=TRUE, col='red')

# What about real-estate?
hist(myreturns[,5], 50, prob=TRUE)
curve(dnorm(x, mean(myreturns[,5]), sd(myreturns[,5])), add=TRUE, col='red')
