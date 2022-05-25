# test sample with no standard deviation
t.test(y, mu=, alternative="")

findStandardizedTestStatistic <- function (sample_x_mean, u_mean, sd, n, round_to = 2) {
    print(round((sample_x_mean - u_mean) / sd * sqrt(n), round_to))
}

findStandardizedTestStatisticAccordingToProb <- function (prob, yes_n, total_n, round_to = 2) {
    print(round((yes_n / total_n - prob) / sqrt(prob * (1 - prob) / total_n), round_to))
}

findPUsingZ <- function (which_taile, z, round_to = 4, a = -1) {
    if (which_taile == "r") {
        print("Right tailed test:")
        p = round(1 - pnorm(z), round_to)
        print(sprintf("P = 1 - pnorm(z) = %s", p))
    } else if (which_taile == "l") {
        print("Left tailed test:")
        p = round(pnorm(z), round_to)
        print(sprintf("P = pnorm(z) = %s", p))
    } else if (which_taile == "t") {
        print("Two tailed test:")
        if (z > 0) {
            p = round(2 * (1 - pnorm(z)), round_to)
            print(sprintf("P = 2 * (1 - pnorm(z)) = %s", p))
        } else {
            p = round(2 * pnorm(z), round_to)
            print(sprintf("P = 2 * pnorm(z) = %s", p))
        }
    }
    if (a > 0) {
        if (p < a) {
            print("Reject H0")
        } else {
            print("Fail to reject H0")
        }
    }
    return (p)
}

comparePWithAlpha <- function (p, alpha, isClaimH0) {
    if (isClaimH0 == 0) {
        if (p < alpha) {
            print("Reject H0, and there is enough evidence to reject the claim.")
        } else {
            print("Fail to reject H0, and there is not enough evidence to reject the claim.")
        }
    } else {
        if (p < alpha) {
            print("Reject H0, so there is enough evidence to support the claim.")
        } else {
            print("Fail to reject H0, and there is not enough evidence to support the claim.")
        }
    }
}

analyzeClaimWithKnownSD <- function (how, u_mean, sample_x_mean, population_sd, n, alpha, round_to = 4) {
    z = (sample_x_mean - u_mean) / population_sd * sqrt(n)
    print(sprintf("standardized test statistic - z = %s", round(z, round_to)))
    if (how == "<=") {
        print(sprintf("H0: <= %s (claim)", u_mean))
        print(sprintf("HA: > %s", u_mean))
        comparePWithAlpha(findPUsingZ("r", z, round_to), alpha, 0)
    } else if (how == ">=") {
        print(sprintf("H0: >= %s (claim)", u_mean))
        print(sprintf("HA: < %s", u_mean))
        comparePWithAlpha(findPUsingZ("l", z, round_to), alpha, 0)
    } else if (how == "==") {
        print(sprintf("H0: == %s (claim)", u_mean))
        print(sprintf("HA: != %s", u_mean))
        comparePWithAlpha(findPUsingZ("t", z, round_to), alpha, 0)
    } else if (how == ">") {
        print(sprintf("H0: <= %s", u_mean))
        print(sprintf("HA: > %s (claim)", u_mean))
        comparePWithAlpha(findPUsingZ("r", z, round_to), alpha, 1)
    } else if (how == "<") {
        print(sprintf("H0: >= %s", u_mean))
        print(sprintf("HA: < %s (claim)", u_mean))
        comparePWithAlpha(findPUsingZ("l", z, round_to), alpha, 1)
    } else if (how == "!=") {
        print(sprintf("H0: == %s", u_mean))
        print(sprintf("HA: != %s (claim)", u_mean))
        comparePWithAlpha(findPUsingZ("t", z, round_to), alpha, 1)
    }
}

analyzeClaimWithSampleAndGivenSD <- function (how, u_mean, sample, population_sd, alpha, round_to = 4) {
    analyzeClaimWithKnownSD(how, u_mean, mean(sample), population_sd, length(sample), alpha, round_to)
}

findPUsingT <- function (which_taile, t, n, round_to = 4, a = -1) {
    if (which_taile == "r") {
        print("Right tailed test:")
        p = round(1 - pt(t, n-1), round_to)
        print(sprintf("P = 1 - pt(t, n-1) = %s", p))
    } else if (which_taile == "l") {
        print("Left tailed test:")
        p = round(pt(t, n-1), round_to)
        print(sprintf("P = pt(t, n-1) = %s", p))
    } else if (which_taile == "t") {
        print("Two tailed test:")
        if (t > 0) {
            p = round(2 * (1 - pt(t, n-1)), round_to)
            print(sprintf("P = 2 * (1 - pt(t, n-1)) = %s", p))
        } else {
            p = round(2 * pt(t, n-1), round_to)
            print(sprintf("P = 2 * pt(t, n-1) = %s", p))
        }
    }
    if (a > 0) {
        if (p < a) {
            print("Reject H0")
        } else {
            print("Fail to reject H0")
        }
    }
    return (p)
}

analyzeClaimWithT <- function (how, u_mean, sample_x_mean, sample_sd, n, alpha, round_to = 4) {
    t = (sample_x_mean - u_mean) / sample_sd * sqrt(n)
    print(sprintf("standardized test statistic - t = %s", round(t, round_to)))
    if (how == "<=") {
        print(sprintf("H0: <= %s (claim)", u_mean))
        print(sprintf("HA: > %s", u_mean))
        comparePWithAlpha(findPUsingT("r", t, n, round_to), alpha, 0)
    } else if (how == ">=") {
        print(sprintf("H0: >= %s (claim)", u_mean))
        print(sprintf("HA: < %s", u_mean))
        comparePWithAlpha(findPUsingT("l", t, n, round_to), alpha, 0)
    } else if (how == "==") {
        print(sprintf("H0: == %s (claim)", u_mean))
        print(sprintf("HA: != %s", u_mean))
        comparePWithAlpha(findPUsingT("t", t, n, round_to), alpha, 0)
    } else if (how == ">") {
        print(sprintf("H0: <= %s", u_mean))
        print(sprintf("HA: > %s (claim)", u_mean))
        comparePWithAlpha(findPUsingT("r", t, n, round_to), alpha, 1)
    } else if (how == "<") {
        print(sprintf("H0: >= %s", u_mean))
        print(sprintf("HA: < %s (claim)", u_mean))
        comparePWithAlpha(findPUsingT("l", t, n, round_to), alpha, 1)
    } else if (how == "!=") {
        print(sprintf("H0: == %s", u_mean))
        print(sprintf("HA: != %s (claim)", u_mean))
        comparePWithAlpha(findPUsingT("t", t, n, round_to), alpha, 1)
    }
}