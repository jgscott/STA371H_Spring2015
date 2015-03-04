library(mosaic)

salary = read.csv('salary.csv', header=TRUE)

# Look at the distibution of salary by sex
mean(Salary~Sex,data=salary)
boxplot(Salary~Sex,data=salary, names=c("Female", "Male"))

# Fit a model
lm3= lm(Salary~Experience+Months+Education+Sex, data=salary)
summary(lm3)

# Permutation test
perm1 = do(1000) * {
  lm_shuffle = lm(Salary~Experience+Months+Education+shuffle(Sex), data=salary)
  lm_shuffle
}
head(perm1)
hist(perm1$Sex, 20)
abline(v=2320.54)

sum(perm1$Sex > 2320.54)
