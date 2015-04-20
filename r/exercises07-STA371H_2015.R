library(mosaic)

# Peak demand
peakdemand = read.csv("peakdemand.csv", header=T)
summary(peakdemand)

# Recast month as a categorical predictor
peakdemand$Month = factor(peakdemand$Month)
boxplot(PeakDemand~Month,data=peakdemand)

# Versus temperature
plot(PeakDemand~DailyTemp,data=peakdemand)

# Over time: clearly rising
plot(peakdemand$PeakDemand, type='l')

# Add a time index to account for the trend
peakdemand$TimeIndex = 1:nrow(peakdemand)

# Weekend effects
lm(PeakDemand~Sat,data=peakdemand)
lm(PeakDemand~Sun,data=peakdemand)

# Fit a model incorporating all these effects
lm1 = lm(PeakDemand ~ TimeIndex + Month + DailyTemp + I(DailyTemp^2) + Sat + Sun, data=peakdemand)
summary(lm1)
anova(lm1)

# Plot fitted values
plot(peakdemand$PeakDemand, type='l')
lines(fitted(lm1), col='red')

# No obvious non-linear effects
plot(PeakDemand ~ fitted(lm1), data=peakdemand)


# Spam filtering
spamfit = read.csv('spamfit.csv', header=TRUE)
spamtest = read.csv('spamtest.csv', header=TRUE)

# Fit the model versus all predictors (. is shorthand for this)
glm1 = glm(y ~ ., data=spamfit, family='binomial')
summary(glm1)

# Make tables to calculate error rates
yhat_train = predict(glm1, newdata=spamfit, type='response')
class_train = yhat_train>0.5
xtabs(~ class_train + spamfit$y)

# Calculate error rates
97/(97+1703) # FPR
321/(321+879)  # FNR
97/(97+879)  # FDR

# Predict on test data
yhat_test = predict(glm1, newdata=spamtest, type='response')
class_test = yhat_test>0.5
xtabs(~ class_test + spamtest$y)

# Calculate error rates on test set
27/(27+358) # FPR
51/(51+165)  # FNR
27/(27+165)  # FDR
