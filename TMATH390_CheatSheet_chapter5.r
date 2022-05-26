"""
CV - critical value / confidence interval. 
"""

# Find z critical value according to the cumulative area:
qnorm(cv + (1 - cv) /2)

# ----- Use the normal distribution to approximate the binomial distribution -----

# approximate the exactly probability: 
normalDistributionApproximateTheExactlyProbability <- function(x, n, p, round_to) {
    print(round(pnorm(x + 0.5, mean = n*p, sd = sqrt(n * p *(1-p))) - pnorm(x - 0.5, mean = n * p, sd = sqrt(n*p *(1-p))), round_to))
}

# approximate the probability between x1 and x2 inclusive: 
normalDistributionapproximateTheProbabilityBetween <- function(x1, x2, n, p, round_to) {
    print(round(pnorm(x2 + 0.5, mean = n*p, sd = sqrt(n * p *(1-p))) - pnorm(x1 - 0.5, mean = n * p, sd = sqrt(n*p *(1-p))), round_to))
}

# at most / not more than x
normalDistributionapproximateTheProbabilityAtMost <- function(x, n, p, round_to) {
    print(round(pnorm(x + 0.5, mean = n*p, sd = sqrt(n*p *(1-p))), round_to))
}

# fewer than x
normalDistributionapproximateTheProbabilityFewerThan <- function(x, n, p, round_to) {
    print(round(pnorm(x - 0.5, mean = n*p, sd = sqrt(n*p *(1-p))), round_to))
}

# at least / not fewer than x
normalDistributionapproximateTheProbabilityAtLeast <- function(x, n, p, round_to) {
    print(round(1- pnorm(x - 0.5, mean = n*p, sd = sqrt(n*p *(1-p))), round_to))
}

# more than x
normalDistributionapproximateTheProbabilityMoreThan <- function(x, n, p, round_to) {
    print(round(1 - pnorm(x + 0.5, mean = n*p, sd = sqrt(n*p *(1-p))), round_to))
}

