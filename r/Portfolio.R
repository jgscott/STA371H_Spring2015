library(mosaic)
library(fImport)

# Import a few stocks
mystocks = c("ORCL", "JNJ", "WMT", "XOM", "MRK")
myprices = yahooSeries(mystocks, from='2009-01-01', to='2015-04-24')

# The first few rows
head(myprices)

# Compute the returns from the closing prices
myreturns = computereturns(myprices)
head(myreturns)

# These returns can be viewed as draws from the joint distribution
pairs(myreturns)
plot(myreturns[,1])

# Simulate a one-day change in your portfolio
totalwealth = 10000
weights = c(0.2, 0.2, 0.2, 0.2, 0.2)	# What percentage of your wealth will you put in each stock?

# How much money do we have in each stock?
holdings = weights * totalwealth

# Sample a random return from the empirical joint distribution
# This simulates a random day
return.today = resample(myreturns, 1, orig.ids=FALSE)

# Update the value of your holdings
holdings = holdings + holdings*return.today

# Compute your new total wealth
totalwealth = sum(holdings)


# Now loop over a whole trading year
totalwealth = 10000
weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
holdings = weights * totalwealth
wealthtracker = rep(0, 250) # Set up a placeholder to track total wealth
for(today in 1:250) {
	return.today = resample(myreturns, 1, orig.ids=FALSE)
	holdings = holdings + holdings*return.today
	totalwealth = sum(holdings)
	wealthtracker[today] = totalwealth
}
totalwealth
plot(wealthtracker)


# Now simulate many different possible trading years!
sim1 = do(100)*{
	totalwealth = 10000
	weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
	holdings = weights * totalwealth
	wealthtracker = rep(0, 250) # Set up a placeholder to track total wealth
	for(today in 1:250) {
		return.today = resample(myreturns, 1, orig.ids=FALSE)
		holdings = holdings + holdings*return.today
		totalwealth = sum(holdings)
		wealthtracker[today] = totalwealth
	}
	totalwealth
}
hist(sim1$result)


# Profit/loss
hist(sim1$result - 10000)

# Calculate 5% value at risk
quantile(sim1$result, 0.05) - 10000


# A helper function for calculating percent returns from a Yahoo Series
# Source this to the console first, and then it will be available to use
# (Like importing a library)
computereturns = function(series) {
	mycols = grep('Adj.Close', colnames(series))
	closingprice = series[,mycols]
	N = nrow(closingprice)
	percentreturn = as.data.frame(closingprice[2:N,]) / as.data.frame(closingprice[1:(N-1),]) - 1
	mynames = strsplit(colnames(percentreturn), '.', fixed=TRUE)
	mynames = lapply(mynames, function(x) return(paste0(x[1], ".PctReturn")))
	colnames(percentreturn) = mynames
	na.omit(percentreturn)
}


