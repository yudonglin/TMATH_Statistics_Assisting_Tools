# 8.1

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
    if (n_2 == -1) {
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

analyzeClaimOfTwoPopulationProportionsBasedOnSamplesWithKnownSD <- function (how, sample_1, population_sd_1, sample_2, population_sd_2, alpha, round_to = 4) {
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
            prop.test(x=c(x_1, x_2), n=c(n_1, n_2), alternative="greater", correct = FALSE)
        } else if (how == ">=") {
            print(sprintf("H0: u1 >= u2 (claim)"))
            print(sprintf("HA: u1 < u2"))
            comparePWithAlpha(findPUsingZ("l", z, round_to), alpha, 0)
            prop.test(x=c(x_1, x_2), n=c(n_1, n_2), alternative="less", correct = FALSE)
        } else if (how == "==") {
            print(sprintf("H0: u1 == u2 (claim)"))
            print(sprintf("HA: u1 != u2"))
            comparePWithAlpha(findPUsingZ("t", z, round_to), alpha, 0)
            prop.test(x=c(x_1, x_2), n=c(n_1, n_2), alternative="two.sided", correct = FALSE)
        } else if (how == ">") {
            print(sprintf("H0: u1 <= u2"))
            print(sprintf("HA: u1 > u2 (claim)"))
            comparePWithAlpha(findPUsingZ("r", z, round_to), alpha, 1)
            prop.test(x=c(x_1, x_2), n=c(n_1, n_2), alternative="greater", correct = FALSE)
        } else if (how == "<") {
            print(sprintf("H0: u1 >= u2"))
            print(sprintf("HA: u1 < u2 (claim)"))
            comparePWithAlpha(findPUsingZ("l", z, round_to), alpha, 1)
            prop.test(x=c(x_1, x_2), n=c(n_1, n_2), alternative="less", correct = FALSE)
        } else if (how == "!=") {
            print(sprintf("H0: u1 == u2"))
            print(sprintf("HA: u1 != u2 (claim)"))
            comparePWithAlpha(findPUsingZ("t", z, round_to), alpha, 1)
            prop.test(x=c(x_1, x_2), n=c(n_1, n_2), alternative="two.sided", correct = FALSE)
        }
    } else {
        print("Cannot continue due to potential input error!")
    }
}

# 8.2

findPUsingTAndDf <- function (which_taile, t, df_in, round_to = 4, a = -1) {
    if (which_taile == "r") {
        print("Right tailed test:")
        p = round(1 - pt(t, df = df_in), round_to)
        print(sprintf("P = 1 - pt(t, df) = %s", p))
    } else if (which_taile == "l") {
        print("Left tailed test:")
        p = round(pt(t, df = df_in), round_to)
        print(sprintf("P = pt(t, df) = %s", p))
    } else if (which_taile == "t") {
        print("Two tailed test:")
        if (t > 0) {
            p = round(2 * (1 - pt(t, df = df_in)), round_to)
            print(sprintf("P = 2 * (1 - pt(t, df)) = %s", p))
        } else {
            p = round(2 * pt(t, df = df_in), round_to)
            print(sprintf("P = 2 * pt(t, df) = %s", p))
        }
    }
    return (p)
}

analyzeClaimOfTwoPopulationProportionsWithUnkownSD <- function (how, equalVariances, sample_mean_1, sample_sd_1, n_1, sample_mean_2, sample_sd_2, n_2, alpha, round_to = 3) {
    if (equalVariances == TRUE) {
        t = (sample_mean_1 - sample_mean_2) / sqrt(((n_1-1) * sample_sd_1 ^2 + (n_2-1) * sample_sd_2 ^2) / (n_1+n_2-2)) / sqrt(1/n_1+1/n_2)
        theDF = n_1+n_2-2
    } else if (equalVariances == FALSE) {
        t = (sample_mean_1 - sample_mean_2) / sqrt(sample_sd_1^2/n_1 + sample_sd_2^2/n_2)
        theDF = (sample_sd_1^2/n_1+sample_sd_2^2/n_2)^2 / ((sample_sd_1^2/n_1)^2 / (n_1-1) + (sample_sd_2^2/n_2)^2 / (n_2-1))
    }
    print(sprintf("standardized test statistic - t = %s", round(t, round_to)))
    print(sprintf("d.f = %s", round(theDF, round_to)))
    if (how == "<=") {
        print("H0: u1 <= u2 (claim)")
        print("HA: u1 > u2")
        comparePWithAlpha(findPUsingTAndDf("r", t, theDF, round_to), alpha, 0)
    } else if (how == ">=") {
        print("H0: u1 >= u2 (claim)")
        print("HA: u1 < u2")
        comparePWithAlpha(findPUsingTAndDf("l", t, theDF, round_to), alpha, 0)
    } else if (how == "==") {
        print("H0: u1 == u2 (claim)")
        print("HA: u1 != u2")
        comparePWithAlpha(findPUsingTAndDf("t", t, theDF, round_to), alpha, 0)
    } else if (how == ">") {
        print("H0: u1 <= u2")
        print("HA: u1 > u2 (claim)")
        comparePWithAlpha(findPUsingTAndDf("r", t, theDF, round_to), alpha, 1)
    } else if (how == "<") {
        print("H0: u1 >= u2")
        print("HA: u1 < u2 (claim)")
        comparePWithAlpha(findPUsingTAndDf("l", t, theDF, round_to), alpha, 1)
    } else if (how == "!=") {
        print("H0: u1 == u2")
        print("HA: u1 != u2 (claim)")
        comparePWithAlpha(findPUsingTAndDf("t", t, theDF, round_to), alpha, 1)
    }
}

analyzeClaimOfTwoPopulationProportionsBasedOnSamplesWithUnknownSD <- function (how, equalVariances, sample1, sample2, alpha, round_to = 3) {
    analyzeClaimOfTwoPopulationProportionsWithUnkownSD(how, equalVariances, mean(sample1), sd(sample1), length(sample1), mean(sample2), sd(sample2), length(sample2), alpha, round_to)
    t.test(sample1, sample2, alternative = c("two.sided"), var.equal = equalVariances, conf.level = 0.95, paired = FALSE)
    if (how == "<=" || how == ">") {
        t.test(sample1, sample2, alternative = c("greater"), var.equal = equalVariances, conf.level = 0.95, paired = FALSE)
    } else if (how == ">=" || how == "<") {
        t.test(sample1, sample2, alternative = c("less"), var.equal = equalVariances, conf.level = 0.95, paired = FALSE)
    } else if (how == "==" || how == "!=") {
        t.test(sample1, sample2, alternative = c("two.sided"), var.equal = equalVariances, conf.level = 0.95, paired = FALSE)
    }
}

y1 = c()
y2 = c()

# test two sample
t.test(y1, y2, alternative = c("two.sided"), var.equal = T, conf.level = 0.95, paired = FALSE)

# test the difference - y2-y1
t.test(y2, y1, alternative = c("greater"), conf.level = 0.95, paired = TRUE)
# test the difference - y1-y2
t.test(y1, y2, alternative = c("greater"), conf.level = 0.95, paired = TRUE)