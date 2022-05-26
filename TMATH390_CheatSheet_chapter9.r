# two samples
x = c()
y = c()

# 8.1

"""
The range of values for the correlation coefficient is -1 to 1, inclusive.

r represents the sample correlation coefficient.

p represents the population correlation coefficient.

"""

# calculate the correlation coefficient
# cor(x, y)

# test the samples and find test statistic t,  p - value, and the correlation coefficient
cor.test(x, y)


# 8.2
"""
coefficient of determination is Multiple R-squared

The coefficient of determination measures the percent of variation explained by the multiple regression model.

The standard error is the expected error of the predicted sales given ...

pay attention is the result is not meaningful

"""

# mod <- lm(y ~ x1 + x2 + x3)
mod <- lm(y ~ x)
summary(mod)

