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

analyzeClaimOfTwoSampleStatisticsWithKnownSD <- function (how, mean_1, n_1, population_sd_1, mean_2, n_2, population_sd_2, alpha, round_to = 4) {
    if (n_2 <= -1) {
        n_2 = n_1
    }
    z = (mean_1 - mean_2) / sqrt(population_sd_1*population_sd_1/n_1+population_sd_2*population_sd_2/n_2)
    print(sprintf("standardized test statistic - z = %s", round(z, round_to)))
    if (how == "<=") {
        print(sprintf("H0: u1 <= u2 (claim)"))
        print(sprintf("HA: u1 > u2"))
        comparePWithAlpha(findPUsingZ("r", z, round_to), alpha, 0)
    } else if (how == ">=") {
        print(sprintf("H0: u1 >= u2 (claim)"))
        print(sprintf("HA: u1 < u2"))
        comparePWithAlpha(findPUsingZ("l", z, round_to), alpha, 0)
    } else if (how == "==") {
        print(sprintf("H0: u1 == u2 (claim)"))
        print(sprintf("HA: u1 != u2"))
        comparePWithAlpha(findPUsingZ("t", z, round_to), alpha, 0)
    } else if (how == ">") {
        print(sprintf("H0: u1 <= u2"))
        print(sprintf("HA: u1 > u2 (claim)"))
        comparePWithAlpha(findPUsingZ("r", z, round_to), alpha, 1)
    } else if (how == "<") {
        print(sprintf("H0: u1 >= u2"))
        print(sprintf("HA: u1 < u2 (claim)"))
        comparePWithAlpha(findPUsingZ("l", z, round_to), alpha, 1)
    } else if (how == "!=") {
        print(sprintf("H0: u1 == u2"))
        print(sprintf("HA: u1 != u2 (claim)"))
        comparePWithAlpha(findPUsingZ("t", z, round_to), alpha, 1)
    }
}

analyzeClaimOfTwoSamplesWithKnownSD <- function (how, sample_1, population_sd_1, sample_2, population_sd_2, alpha, round_to = 4) {
    analyzeClaimOfTwoSampleStatisticsWithKnownSD(how, mean(sample_1), length(sample_1), population_sd_1, mean(sample_2), length(sample_2), population_sd_2, alpha, round_to)
}

analyzeClaimOfTwoPopulationProportions <- function (how, x_1, n_1, x_2, n_2, alpha, round_to = 4) {
    if (x_1 < n_1 & x_2 < n_2){
        z = ((x_1/n_1)-(x_2/n_2)) / sqrt((x_1+x_2) / (n_1+n_2) * (1 - (x_1+x_2)/(n_1+n_2)) * (1/n_1+1/n_2))
        print(sprintf("standardized test statistic - z = %s", round(z, round_to)))
        if (how == "<=") {
            print(sprintf("H0: u1 <= u2 (claim)"))
            print(sprintf("HA: u1 > u2"))
            comparePWithAlpha(findPUsingZ("r", z, round_to), alpha, 0)
        } else if (how == ">=") {
            print(sprintf("H0: u1 >= u2 (claim)"))
            print(sprintf("HA: u1 < u2"))
            comparePWithAlpha(findPUsingZ("l", z, round_to), alpha, 0)
        } else if (how == "==") {
            print(sprintf("H0: u1 == u2 (claim)"))
            print(sprintf("HA: u1 != u2"))
            comparePWithAlpha(findPUsingZ("t", z, round_to), alpha, 0)
        } else if (how == ">") {
            print(sprintf("H0: u1 <= u2"))
            print(sprintf("HA: u1 > u2 (claim)"))
            comparePWithAlpha(findPUsingZ("r", z, round_to), alpha, 1)
        } else if (how == "<") {
            print(sprintf("H0: u1 >= u2"))
            print(sprintf("HA: u1 < u2 (claim)"))
            comparePWithAlpha(findPUsingZ("l", z, round_to), alpha, 1)
        } else if (how == "!=") {
            print(sprintf("H0: u1 == u2"))
            print(sprintf("HA: u1 != u2 (claim)"))
            comparePWithAlpha(findPUsingZ("t", z, round_to), alpha, 1)
        }
    } else {
        print("Cannot continue due to potential input error!")
    }
}

prop.test(x=c(x1, x2), n=c(n1, n2), alternative="", correct = FALSE)