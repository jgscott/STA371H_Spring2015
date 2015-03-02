# Problem 1

# Load the mosaic library
library(mosaic)
library(faraway)
data(mammalsleep, package="faraway")

# Part A
# Clearly we need a log transform of both variables
plot(brain ~ body, data=mammalsleep)
plot(log(brain) ~ log(body), data=mammalsleep)

# Fit a model:
lm1 = lm(log(brain) ~ log(body), data=mammalsleep)
coef(lm1)
# The fitted model is log(y) = 2.13 + 0.75*log(x)
# This should be re-expressed as a power law on the original scale

# Residual uncertainty:
sd(resid(lm1))

# Part B: Largest and smallest brains
# Can look manually
mammalsleep$brain

# or use which.max, which.min
which.max(mammalsleep$brain)
which.min(mammalsleep$brain)
mammalsleep[1,]
mammalsleep[32,]

# Largest and smallest residuals
# These are the largest/smallest brains adjusting for body size
resid(lm1)
which.max(resid(lm1))
which.min(resid(lm1))


# Part C: plug-in prediction
coef(lm1)
new_body = 100
# Plug into equation
log_yhat = 2.13 + 0.75 * log(new_body)
# Invert the transformation
yhat = exp(log_yhat)

# To get a whole interval, we need to do this with both endpoints
# We will get an approximate 95% prediction interval by extending
# two residual standard deviations in either direction on the log scale,
# then inverting the log transformation.
sigma = sd(resid(lm1))
log_lower_bound = 2.13 + 0.75 * log(new_body) - 2*sigma
log_upper_bound = 2.13 + 0.75 * log(new_body) + 2*sigma
exp(log_lower_bound)
exp(log_upper_bound)


# Problem 2: import utilities data set
# Start from utilities walkthrough

# Define daily average gas gill
daily.average.gasbill = utilities$gasbill/utilities$billingDays

# Fit a model with a quadratic term:
lm1=lm(daily.average.gasbill ~ temp , data=utilities)
plot(daily.average.gasbill ~ temp, data=utilities)
points(fitted(lm1)~temp, data=utilities, col='red', pch=19)


# Fit a model with a quadratic term:
lm2=lm(daily.average.gasbill ~ temp + I(temp^2), data=utilities)
plot(daily.average.gasbill ~ temp, data=utilities)
points(fitted(lm2)~temp, data=utilities, col='blue', pch=19)

# Fit a model with a cubic term:
lm3=lm(daily.average.gasbill ~ temp + I(temp^2) + I(temp^3), data=utilities)
plot(daily.average.gasbill ~ temp, data=utilities)
points(fitted(lm3)~temp, data=utilities, col='green', pch=19)

# Fit a model with a 4th-order term:
lm4=lm(daily.average.gasbill ~ temp + I(temp^2) + I(temp^3) + I(temp^4), data=utilities)
plot(daily.average.gasbill ~ temp, data=utilities)
points(fitted(lm4)~temp, data=utilities, col='grey', pch=19)

# The linear model is obviously poor, but none of the others is obviously a better fit than the other.  I'll choose the quadratic.
plot(daily.average.gasbill ~ temp, data=utilities)
points(fitted(lm1)~temp, data=utilities, col='red', pch=19)
points(fitted(lm2)~temp, data=utilities, col='blue', pch=19)

# Prediction intervals...
# For the linear model:
sigma_lm1 = sd(resid(lm1))
new_temp = 50
yhat_1 = 7.34761655 - 0.09643241*new_temp
# Two standard deviations to either side
yhat_1 - 2*sigma_lm1
yhat_1 + 2*sigma_lm1
# Calculate empirical coverage
sum(resid(lm1) > 2*sigma_lm1) # misses low
sum(resid(lm1) < -2*sigma_lm1) # misses high
4/117 # fraction of misses out of 117 data points

# For the quadratic model:
sigma_lm2 = sd(resid(lm2))
new_temp = 50
coef(lm2)
yhat_2 = 9.472288520 - 0.211555282*new_temp + 0.001247559 * new_temp^2
# Two standard deviations to either side
yhat_2 - 2*sigma_lm2
yhat_2 + 2*sigma_lm2
# Calculate empirical coverage
sum(resid(lm2) > 2*sigma_lm2) # misses low
sum(resid(lm2) < -2*sigma_lm2) # misses high
12/117 # fraction of misses out of 117 data points


# Problem 3: read in 10 mile race data
library(mosaicData)
data(TenMileRace)

# The model aggregrating men and women
plot(net~age,data=TenMileRace, col='grey')
lm1 = lm(net~age,data=TenMileRace)
abline(lm1)
summary(lm1)

# Now disaggregating
lmM = lm(net~age,data=subset(TenMileRace,sex=="M"))
lmF = lm(net~age,data=subset(TenMileRace,sex=="F"))
summary(lmM)
summary(lmF)

# Clearly an aggregation paradox