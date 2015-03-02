# Problem 1
# Load the oxford data set using "Import Dataset" button
# Make sure to click on the button indicating there is a header row

# Load the mosaic library
library(mosaic)

# A box plot
bwplot(Price ~ factor(Year), data=oxford)

# The groupwise means
mean(Price ~ factor(Year), data=oxford)

# Two deficiencies of the groupwise model:
# 1) Nothing relates adjacent years, which ought to be similar.
# 2) We cannot forecast/look ahead: no estimate of trend.



# Problem 2

# A: first calculate average price per neighborhood
nbhd_means = mean(Price ~ Neighborhood, data=afc)
nbhd_means
# Looks like Convention Center cheapest, Congress Ave. Area most expensive
# Annoyingly, there are two "Convention Center" neighborhoods... a typo in the data set

# B: fit models and look at plots
plot(Price ~ FoodScore, data=afc)
lm1 = lm(Price ~ FoodScore, data=afc)
plot(Price ~ FeelScore, data=afc)
lm2 = lm(Price ~ FeelScore, data=afc)

# Check which model has a smaller residual standard deviation
sd(resid(lm1))
sd(resid(lm2))
# Looks like FoodScore is a stronger predictor

# C: use residuals from model for Price versus FoodScore
# This is price, adjusted for food quality
value_measure = resid(lm1)
mean(value_measure ~ Neighborhood, data=afc)
# Looks like Congress Ave. Area has the highest average residual
# East Austin the lowest average residual


# Problem 3

# Read in data first

# Part A: create models
lmAAPL = lm(AAPL~SP500,data=marketmodel)
lmGOOG = lm(GOOG~SP500,data=marketmodel)
lmMRK = lm(MRK~SP500,data=marketmodel)
lmJNJ = lm(JNJ~SP500,data=marketmodel)
lmWMT = lm(WMT~SP500,data=marketmodel)
lmTGT = lm(TGT~SP500,data=marketmodel)

# Extract information from each model
# E.g. for Apple:
coef(lmAAPL)
sd(resid(lmAAPL))

# And so on for the remaining stocks

# Which stock seems to be the most tightly coupled to the movements of the wider market?
# No one right answer; e.g. could use the highest slopes or the lowest residual standard deviations to support an argument here

# Part D: look at Wal-Mart residuals versus other residuals
plot(resid(lmWMT) ~ resid(lmTGT))
plot(resid(lmWMT) ~ resid(lmAAPL))
plot(resid(lmWMT) ~ resid(lmGOOG))
plot(resid(lmWMT) ~ resid(lmMRK))
plot(resid(lmWMT) ~ resid(lmJNJ))

# WMT residuals look nearly uncorrelated with AAPL and GOOG residuals
# To compare the other three, we could fit models for the WalMart residuals:
lm1 = lm(resid(lmWMT) ~ resid(lmTGT))
lm2 = lm(resid(lmWMT) ~ resid(lmMRK))
lm3 = lm(resid(lmWMT) ~ resid(lmJNJ))

# Model 1 has the lowest residual standard deviation
# This is good evidence for the claim.
sd(resid(lm1))
sd(resid(lm2))
sd(resid(lm3))
