# Load in the fImport library (install it first)
library(fImport)

# Pick a stock
my_tickers = c("AAPL")

# Import the data from the web
stock_data = yahooSeries(my_tickers, from = "2011-01-01", to = "2015-01-21")

# Plot the data
plot(stock_data)

# Calculate the intra-day spread as a percentage of closing price
intraday_spread = (stock_data$AAPL.High - stock_data$AAPL.Low)/stock_data$AAPL.Close
plot(intraday_spread, type='l')

# Look at the first few lines
head(stock_data)
