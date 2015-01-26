# Load in the relevant libraries
library(mosaic)

# Now load the ut2000.csv data set

# A box-and-whisker plot of SAT Math vs college
bwplot(SAT.Q ~ School, data=ut2000)

# Group-wise means for SAT Verbal
mean(SAT.V ~ School, data=ut2000)

# A scatter plot showing GPA versus SAT Math
plot(GPA ~ SAT.Q, data=ut2000)

# A lattice plot showing GPA versus SAT Math stratified by college
xyplot(GPA ~ SAT.Q | School, data=ut2000)
