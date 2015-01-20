# Anything followed by the #-sign is a comment.
# It is ignored by R.

# You can use R as a graphing calculator
# Type these commands directly into the console.
2 + 2
3*7
2^3
log10(100)

#### In Galton's footsteps
# Read in the data set heights.csv using the Import Dataset button

# Get a summary of each variable in the data set
summary(heights)

# Compute the mean and standard deviation of the SHGT variable
mean(heights$SHGT)
sd(heights$SHGT)

# A histogram
hist(heights$SHGT)

# A scatter plot
plot(SHGT ~ MHGT, data=heights)

# You can store the results of computations in new variables
mu = mean(heights$SHGT)
sigma = sd(heights$SHGT)

# Now these variables are availble to be used in subsequent computations.
mu - 2*sigma
mu + 2*sigma

# You can get help about any R command using the question mark
?mean
?sd
