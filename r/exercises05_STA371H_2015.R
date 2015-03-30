library(mosaic)

# Problem 2
cheese = read.csv('cheese.csv')

# Explore the data
summary(cheese)

# Part A

# Clearly volume needs a log transform, and is correlated with displays
boxplot(vol~disp, data=cheese)
boxplot(log(vol)~disp, data=cheese)

# First try: fit a model
lm1 = lm(log(vol)~disp, data=cheese)
summary(lm1)
confint(lm1)

# This is on a log scale
# to get percent increase, exponentiate
exp(confint(lm1))

# However, look store by store
boxplot(log(vol)~disp, data=subset(cheese, store=='BALTI/WASH - SAFEWAY'))
boxplot(log(vol)~disp, data=subset(cheese, store=='SYRACUSE - PRICE CHOPPER'))

# Different stores have very different sales volumes.
# Might be a confounder.
# Put in dummy variables store by store.
lm2 = lm(log(vol)~disp + store, data=cheese)
coef(lm2)
confint(lm2)
# The confidence interval for percent increase based on this model
exp(c(0.30215602,  0.35963022))

# Compare with lm1: it is a lower estimated effect
# Therefore important to account for store-level variability
exp(confint(lm1))

# Part B
# Now look at price
xyplot(log(vol) ~ price | disp, data=cheese)
xyplot(log(vol) ~ price | disp, data=cheese)


# Fit a model that accounts for price
# Fitting a demand curve, like in the milk data set
# Except now we have dummies for store and display that shift the curve up/down
lm3 = lm(log(vol)~log(price) + store + disp, data=cheese)
confint(lm3)

# Our confidence interval has moved down sharply when we adjust for price
# Probably because displays are associated with lower (sale) prices
exp(c(0.16471722, 0.206081192))

# Could also do this
exp(confint(lm3)['disp',])

# Spot check for a single store
# Specifically looking at Kroger DFW
dfwkroger = subset(cheese, store=='DALLAS/FT. WORTH - KROGER CO')
plot(vol~price, data=dfwkroger, las=1)
points(vol~price, data=subset(dfwkroger, disp==1), col='blue', pch=19)
points(vol~price, data=subset(dfwkroger, disp==0), col='red', pch=19)

# Go back and make the curves from the fitted coefficients
# No displays
curve(exp(9.37579 + 1.43461)*x^(-2.53159), add=TRUE, col='red')
# With displays
curve(exp(9.37579 + 1.43461 + 0.18540)*x^(-2.53159), add=TRUE, col='blue')

# Part C: does the elasticity, i.e. the slope on log(price), change
# in the presence of a display?

# Try a model with an interaction
lm4 = lm(log(vol)~log(price) + store + disp + disp:log(price), data=cheese)
coef(lm4)

plot(vol~price, data=dfwkroger, las=1)
points(vol~price, data=subset(dfwkroger, disp==1), col='blue', pch=19)
points(vol~price, data=subset(dfwkroger, disp==0), col='red', pch=19)

# Go back and make the curves from the fitted coefficients
# No displays
curve(exp(9.37579 + 1.43461)*x^(-2.53159), add=TRUE, col='red')
# With displays
curve(exp(9.37579 + 1.43461 + 0.18540)*x^(-2.53159), add=TRUE, col='blue')


# Can we real out zero as a plausible value for the interaction?
confint(lm4)

# The interaction term is probably not zero.
# This suggests a change in elasticity (slope) when disp=1.


### Problem 3: utilities

# Define the daily_gas_bill variable
utilities$daily_gas_bill = utilities$gasbill/utilities$billingDays
n_total = 117

# Choose how large the training set will be out of 117 total data points
n_train = 90


cv1 = do(250)*{
  # Randomly pick points that will form the training set
  # Split the data into the training and testing sets
  training_points = sample(1:n_total, size=n_train, replace=FALSE)
  training_data = utilities[training_points,]
  testing_data = utilities[-training_points,]
  
  # Fit the models to the training set
  model1 = lm(daily_gas_bill ~ temp, data=training_data)
  model2 = lm(daily_gas_bill ~ temp + I(temp^2), data=training_data)
  model3 = lm(daily_gas_bill ~ temp + I(temp^2) + I(temp^3), data=training_data)
  model4 = lm(daily_gas_bill ~ temp + I(temp^2) + I(temp^3) + I(temp^4), data=training_data)
  
  # Predict on the test set and check the errors
  yhat_model1 = predict(model1, newdata=testing_data)
  MSE_model1 = mean( (yhat_model1 - testing_data$daily_gas_bill)^2 )
  yhat_model2 = predict(model2, newdata=testing_data)
  MSE_model2 = mean( (yhat_model2 - testing_data$daily_gas_bill)^2 )
  yhat_model3 = predict(model3, newdata=testing_data)
  MSE_model3 = mean( (yhat_model3 - testing_data$daily_gas_bill)^2 )
  yhat_model4 = predict(model4, newdata=testing_data)
  MSE_model4 = mean( (yhat_model4 - testing_data$daily_gas_bill)^2 )
  
  c(MSE_model1, MSE_model2, MSE_model3, MSE_model4)
}

# Visualize the results
boxplot(cv1)

