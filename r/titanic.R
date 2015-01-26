# Load in the relevant libraries
library(mosaic)

# Now load the TitanicSurvival.csv data set
# using the Import Dataset button in the Environment tab

# The variable names
names(TitanicSurvival)

# To view the data in RStudio,
# double click on it in the Environment tab,
# or just type the name of the data set.
TitanicSurvival

# Look at the first few lines
head(TitanicSurvival)

# Make tables that shows who survived, stratified by sex and cabin class
xtabs(~survived + sex, data=TitanicSurvival)
xtabs(~survived + passengerClass, data=TitanicSurvival)

# Turn a table of counts into a table of proportions
table1 = xtabs(~survived + sex, data=TitanicSurvival)
prop.table(table1, margin=1)
prop.table(table1, margin=2)

# Odds of surviving for males and females
odds_male = 0.19/0.81
odds_female = 0.73/0.27

# Compute the odds ratio
odds_ratio = odds_female/odds_male

# Now survival stratified by two variables
xtabs(~survived + sex + passengerClass, data=TitanicSurvival)

# Boxplot of age versus other variables
bwplot(age~survived, data=TitanicSurvival)
bwplot(age~passengerClass + sex, data=TitanicSurvival)

# Try some others!
