# Load in the relevant libraries
library(mosaic)

### 1: shocks

# Part A
lm1 = lm(expensive~cheap, data=shocks)
summary(lm1)
# R-squared exceeds 90%

# Part B: follow newspapers walkthrough.

# Is beta1 = 1 inside the 95% confidence interval?
confint(lm1, level=0.95)

# Now for the prediction intervals
# Make a data frame for the new x's where you want to predict
new_shocks = data.frame(cheap = c(510, 550, 590))

# Are any of the prediction intervals wider than 33 units?
predict(lm1, new_shocks, interval='prediction', level = 0.95)

# Check the assumptions of the normal regression model
hist(resid(lm1))
plot(resid(lm1) ~ cheap, data=shocks)


## Problem 2
# Read in georgia20002000 data set

names(georgia2000)

# First create an undercount variable as a percentage
# undercount divided by total number of ballots
georgia2000$undercount = 100*(georgia2000$ballots - georgia2000$votes)/georgia2000$ballots

# Create a variable for the Republican vote share
georgia2000$repshare = georgia2000$bush/georgia2000$ballots

# Exploratory analysis: plots help a lot
boxplot(undercount ~ equip, data=georgia2000)
boxplot(undercount ~ poor, data=georgia2000)
boxplot(undercount ~ urban, data=georgia2000)
boxplot(undercount ~ atlanta, data=georgia2000)
plot(undercount ~ perAA, data=georgia2000)
plot(undercount ~ repshare, data=georgia2000)


# One strategy is to start with a model and try to prune
lm1 = lm(undercount ~ poor + urban + atlanta + perAA + repshare + equip, data=georgia2000)
summary(lm1)
anova(lm1)
confint(lm1)

third_party = (georgia2000$votes - georgia2000$bush - georgia2000$gore)/georgia2000$votes
plot(undercount ~ third_party, data=georgia2000)


# Notice that perAA and repshare are highly correlated
plot(perAA ~ repshare, data=georgia2000)

# We will drop one of these variables
lm2 = lm(undercount ~ poor + urban + atlanta + repshare + equip, data=georgia2000)
summary(lm2)
anova(lm2)

# The effect of equipment seems present... what happens if we drop it?
lm3 = lm(undercount ~ poor + urban + atlanta + repshare, data=georgia2000)
summary(lm3)
anova(lm3)

# R-sqared goes down by 7%... is this "significant" or not?
# New idea: use a permutation test
# (not part of the homework!)

# Shuffle the equipment variable once
lm_perm = lm(undercount ~ poor + urban + atlanta + repshare + shuffle(equip), data=georgia2000)
summary(lm_perm)

# Now repeat
perm_test = do(1000)*{
  lm_perm = lm(undercount ~ poor + urban + atlanta + repshare + shuffle(equip), data=georgia2000)
  lm_perm
}

head(perm_test)
hist(perm_test$r.squared)

# Compare with R-squared from the model with the real equipment variable
summary(lm2)

# Compute a p-value
sum(perm_test$r.squared > 0.2729)

# Compare with the normal-theory F test
anova(lm2)


### Problem 3: CIS and beauty ratings

# Plots: ratings seem higher for men, for non-minorities,
# and for pretty people
plot(eval ~ beauty, data=profs)
boxplot(eval ~ minority, data=profs)
plot(eval ~ age, data=profs)
boxplot(eval ~ gender, data=profs)

# Ratings are lower for upper-division courses
# higher for single-credit courses
# higher for non-tenure-track teachers
# and higher for native English speakers
boxplot(eval ~ division, data=profs)
plot(eval ~ log(students), data=profs)
boxplot(eval ~ credits, data=profs)
boxplot(eval ~ tenure, data=profs)
boxplot(eval ~ native, data=profs)


# Let's build a model that incorporates all these effects
lm1 = lm(eval ~ native + tenure + credits + log(students) + gender + minority + beauty, data=profs)
summary(lm1)
anova(lm1)

# It looks like the beauty variable is the single largest contributor to the predictive abilities of the model

# We should check whether this finding is robust.
# What happens if we drop some of the more marginal variables?

# Class size looked like it had a small effect
lm2 = lm(eval ~ native + tenure + credits + gender + minority + beauty, data=profs)
summary(lm2)
anova(lm2)

# The tenure-track variable also has a small effect
# Try dropping it
lm3 = lm(eval ~ native + credits + gender + minority + beauty, data=profs)
summary(lm3)
anova(lm3)
confint(lm3)

lm4 = lm(eval ~ native + credits + gender + minority + beauty + age + division, data=profs)
summary(lm4)

# In all these different models, the "beauty effect" looks present
