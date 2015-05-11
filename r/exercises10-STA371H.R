library(fImport)
library(mosaic)

# Import a few stocks
mytickers = c("SPY", "TLT", "LQD", "DBC", "VNQ")
myprices = yahooSeries(mytickers, from='2006-02-06', to='2015-04-30')

plot(myprices$SPY.Adj.Close, type='l')
plot(myprices$TLT.Adj.Close, type='l')
plot(myprices$LQD.Adj.Close, type='l')
plot(myprices$DBC.Adj.Close, type='l')
plot(myprices$VNQ.Adj.Close, type='l')

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

myreturns = computereturns(myprices)
head(myreturns)
colMeans(myreturns)


# Part 1: VaR, comparing expected utilities

# A single trading year

# Initialize
totalwealth = 10000
weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
holdings = weights * totalwealth
# Simulate wealth at the end of the year
for(today in 1:250) {
  return.today = resample(myreturns, 1, orig.ids=FALSE)
  holdings = holdings + holdings*return.today
  totalwealth = sum(holdings)
  # Re-balance
  holdings = weights * totalwealth
}
totalwealth
# Calculate utility
log(totalwealth)

# Value at risk
# Now simulate many different possible two-week trading windows
sim1 = do(500)*{
  # Initialize
  totalwealth = 10000
  weights = c(0.5, 0.5, 0,0, 0)
  holdings = weights * totalwealth
  # Simulate wealth at the end of the year
  for(today in 1:10) {
    return.today = resample(myreturns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
  }
  # Output total wealth 
  totalwealth
}
hist(sim1$result)

profit = sim1$result - 10000
# Estimated value at risk
quantile(profit, 0.05)


# Compare expected utilities
# Now simulate many different possible trading years
sim1 = do(100)*{
  # Initialize
  totalwealth = 10000
  weights = c(0.5, 0.5, 0,0, 0)
  holdings = weights * totalwealth
  # Simulate wealth at the end of the year
  for(today in 1:250) {
    return.today = resample(myreturns, 1, orig.ids=FALSE)
    holdings = holdings + holdings*return.today
    totalwealth = sum(holdings)
    # Re-balance
    holdings = weights * totalwealth
  }
  # Calculate utility
  log(totalwealth)
}
hist(sim1$result)
mean(sim1$result)




## Peak demand forecasting

# Import peak demand data set

# Add a time index
peakdemand$TimeIndex = 1:nrow(peakdemand)

# Fit a model incorporating all the effects we discovered last time
lm1 = lm(PeakDemand ~ TimeIndex + factor(Month) + DailyTemp + I(DailyTemp^2) + Sat + Sun, data=peakdemand)
summary(lm1)
sigma = 106

# Import new data set
summary(Temp50Year)

# Create a new data set for prediction, assuming every day's temperature is the historical mean
peakdemand_new = Temp50Year
peakdemand_new$DailyTemp = Temp50Year$Mean50Year
# Add time index, starting at 1462
peakdemand_new$TimeIndex = (nrow(peakdemand) + 1):(nrow(peakdemand) + nrow(peakdemand_new))

summary(peakdemand_new)
yhat_new = predict(lm1, peakdemand_new)

# Plot forecast on same plot as old data
plot(PeakDemand ~ TimeIndex, data=peakdemand, xlim=c(0, 2191), type='l')
lines(fitted(lm1), col='red')

# Add forecast for future
lines(yhat_new ~ peakdemand_new$TimeIndex, col='blue')


# Problem 1: I have accounted for the residuals
# One solution: add simulated residual to every point
y_new = yhat_new + rnorm(nrow(peakdemand_new), 0, sigma)

# Plot forecast on same plot as old data
plot(PeakDemand ~ TimeIndex, data=peakdemand, xlim=c(0, 2191), type='l')
lines(fitted(lm1), col='red')

# Add simulated value for future
lines(y_new ~ peakdemand_new$TimeIndex, col='blue')

# Problem 2: I haven't account for uncertainty in temperature
# One solution: simulate future temperatures from their historical distributions


temp_sim = rnorm(nrow(peakdemand_new), peakdemand_new$Mean50Year, peakdemand_new$SD50Year)
plot(temp_sim, col='red', type='l')
lines(peakdemand_new$Mean50Year)

# Add randomly simulated temperatures to data set
temp_sim = rnorm(nrow(peakdemand_new), peakdemand_new$Mean50Year, peakdemand_new$SD50Year)
peakdemand_new$DailyTemp = temp_sim

# Make forecast under simulated temperatures
yhat_new = predict(lm1, peakdemand_new)

# Add simulated residual
y_new = yhat_new + rnorm(nrow(peakdemand_new), 0, sigma)

sum(y_new > 5000)

# Plot forecast on same plot as old data
plot(PeakDemand ~ TimeIndex, data=peakdemand, xlim=c(0, 2191), type='l')
lines(fitted(lm1), col='red')

# Add simulated value for future
lines(y_new ~ peakdemand_new$TimeIndex, col='blue')


## Put this all together in a Monte Carlo simulation

mc1 = do(1000)*{
  # Simulate temperatures
  temp_sim = rnorm(nrow(peakdemand_new), peakdemand_new$Mean50Year, peakdemand_new$SD50Year)
  peakdemand_new$DailyTemp = temp_sim
  
  # Make forecast under simulated temperatures
  yhat_new = predict(lm1, peakdemand_new)
  
  # Add simulated residual
  y_new = yhat_new + rnorm(nrow(peakdemand_new), 0, sigma)
  
  # Calculate number of threshold exceedances
  sum(y_new > 5000)
}

# A table of the results
xtabs(~mc1$result)


# For another class: an issue we might catch
cor(resid(lm1)[1:1460], resid(lm1)[2:1461])
