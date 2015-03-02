# Load in the relevant libraries
library(mosaic)

### 1A: Soldering

# Starting with main effects only
lm1 = lm(skips ~ Opening + Solder + Mask, data=solder)
summary(lm1)

# All interactions
lm2 = lm(skips ~ Opening + Solder + Mask + Solder:Mask + Opening:Solder + Opening:Mask, data=solder)
summary(lm2)

# Looks like a noticeable jump in R^2 when we add the interaction terms
# An analysis of variance will help
anova(lm2)

# Solder:Mask has a low contribution to sums of squares
# Let's try dropping it
lm3 = lm(skips ~ Opening + Solder + Mask + Opening:Solder + Opening:Mask, data=solder)
summary(lm3)

# R-squared drops by 2%... up to you whether this is practically significant or not!

### Part B: Life Expectancy

# Look at life expectancy versus GDP by group
xyplot(LifeExp ~ PPGDP | Group, data=LifeExpectancy)

# Clearly need a log transform of the predictor
xyplot(LifeExp ~ log(PPGDP) | Group, data=LifeExpectancy)

# We obviously need dummy variables to account for the shifts up and down by group
lm1 = lm(LifeExp ~ log(PPGDP) + Group, data=LifeExpectancy)
summary(lm1)

# Does an interaction term change things much?
lm2 = lm(LifeExp ~ log(PPGDP) + Group + log(PPGDP):Group , data=LifeExpectancy)
summary(lm2)

# R-squared barely moves... probably don't need the interaction term


### Problem 2
### Monte Carlo simulation: sampling distributions
### Closely follows the gone fishing walkthrough

# The sample
plot(y~x, data=simdata_samp)
lm1 = lm(y~x, data= simdata_samp)
abline(lm1)
coef(lm1)

# The population
plot(y~x, data=simdata_pop)
lmpop = lm(y~x, data=simdata_pop)
abline(lmpop, col='red', lwd=5)
coef(lmpop)

# A Monte Carlo simulation of the sampling distribution of the OLS estimator
my_sim = do(1000)*{
  this_sample = sample(simdata_pop, 50)
  lmsamp = lm(y ~ x, data=this_sample)
  coef(lmsamp)
}
head(my_sim)

# The standard error of the slope estimate is about 0.1
hist(my_sim$x, 20)
sd(my_sim$x)

# The standard error of the intercept estimate is about 0.6
hist(my_sim$Intercept, 20)
sd(my_sim$Intercept)



### Problem 3: Bootstrapping
### Closely follows the "Creatinine, revisited" walkthrough

# CAPM
plot(WMT ~ SP500, data=marketmodel)
lm_WMT = lm(WMT ~ SP500, data=marketmodel)
abline(lm_WMT)
coef(lm_WMT)

# Estimate sampling distribution by bootstrapping
my_boot = do(1000)*{
  bootstrap_sample = resample(marketmodel)
  lm_boot = lm(WMT ~ SP500, data=bootstrap_sample)
  coef(lm_boot)
}

# Notice that Yahoo's beta is far away from the sampling distribution
hist(my_boot$SP500)
sd(my_boot$SP500)
confint(my_boot, level=0.95)

# Part B
# UT 2000 SAT scores
bwplot(SAT.Q ~ School, data=ut2000)
lm1 = lm(SAT.Q ~ School, data=ut2000)
coef(lm1)

# Bootstrap the group-wise model
my_boot = do(1000)*{
  lm_boot = lm(SAT.Q ~ School, data=resample(ut2000))
  coef(lm_boot)
}
head(my_boot)

# The dummy variable for liberal arts gets us the difference
# between liberal arts and architecture
hist(my_boot$SchoolLIBERAL.ARTS, 25)
confint(my_boot, level=0.95)
sd(my_boot$SchoolLIBERAL.ARTS)


# Could also bootstrap the subsets individually
subset1 = subset(ut2000, School=='ARCHITECTURE')
subset2 = subset(ut2000, School=='LIBERAL ARTS')
my_boot = do(1000)*{
  boot1 = resample(subset1)
  boot2 = resample(subset2)
  mean(boot1$SAT.Q) - mean(boot2$SAT.Q)
}
hist(my_boot$result)
sd(my_boot$result)

