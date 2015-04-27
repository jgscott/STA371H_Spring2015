library(mosaic)

## Monte Carlo tips and tricks

# Conditional statements work the same as in Excel
x = rnorm(1, mean=0, sd=1)
if(x > 0) {y = 1} else {y = -1}

# You can also split these statements across multiple lines,
# as long as the 'else' is on the same line as the trailing brace from 'if'
x = rnorm(1, mean=0, sd=1)
if(x > 0) {
  y = 1
} else {
  y = -1
}
y

# You can flip a biased coin in at least three different ways.

# First, draw uniform random numbers and threshold appropriately
sim1 = do(1000)*{
  x = runif(1, min=0, max=1)
  if(x < 0.7) {
    y = 1
  } else {
    y = -1
  }
  y
}

# Check the resulting simulated values
table(sim1$result)


# Second, sample from a binomial distribution
sim2 = do(1000)*{
  x = rbinom(1, size=1, prob=0.7)
  if(x == 1) {
    y = -0.35
  } else {
    y = 0.5
  }
  y
}

# Check the resulting simulated values
table(sim2$result)


# Third, use the sample function
mychoices = c(-1,1)
myprobs = c(0.3, 0.7)
sim3 = do(1000)*{
  y = sample(mychoices, size=1, prob=myprobs)
  y
}

# Check the resulting simulated values
table(sim3$result)

