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

analyzeClaimWithKnownSD <- function (how, num, sample_x_mean, u_mean, sd, n, alpha, round_to = 4) {
    z = (sample_x_mean - u_mean) / sd * sqrt(n)
    print(sprintf("z = %s", round(z, round_to)))
    if (how == "<=") {
        print(sprintf("H0: <= %s (claim)", num))
        print(sprintf("HA: > %s", num))
        if (findPUsingZ("r", z, round_to) < alpha) {
            print("Reject H0 as well as the claim")
        } else {
            print("Fail to reject H0 as well as the claim")
        }
    } else if (how == ">=") {
        print(sprintf("H0: >= %s (claim)", num))
        print(sprintf("HA: < %s", num))
        if (findPUsingZ("l", z, round_to) < alpha) {
            print("Reject H0 as well as the claim")
        } else {
            print("Fail to reject H0 as well as the claim")
        }
    } else if (how == "==") {
        print(sprintf("H0: == %s (claim)", num))
        print(sprintf("HA: != %s", num))
        if (findPUsingZ("t", z, round_to) < alpha) {
            print("Reject H0 as well as the claim")
        } else {
            print("Fail to reject H0 as well as the claim")
        }
    } else if (how == "<") {
        print(sprintf("H0: >= %s", num))
        print(sprintf("HA: < %s (claim)", num))
        if (findPUsingZ("l", z, round_to) < alpha) {
            print("Reject H0, but support the claim")
        } else {
            print("Fail to reject H0, and there is not enough evidence to support the claim.")
        }
    } else if (how == ">") {
        print(sprintf("H0: <= %s", num))
        print(sprintf("HA: > %s (claim)", num))
        if (findPUsingZ("r", z, round_to) < alpha) {
            print("Reject H0, but support the claim")
        } else {
            print("Fail to reject H0, and there is not enough evidence to support the claim.")
        }
    } else if (how == "!=") {
        print(sprintf("H0: == %s", num))
        print(sprintf("HA: != %s (claim)", num))
        if (findPUsingZ("t", z, round_to) < alpha) {
            print("Reject H0, but support the claim")
        } else {
            print("Fail to reject H0, and there is not enough evidence to support the claim.")
        }
    }
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

analyzeClaimWithT <- function (how, num, sample_x_mean, u_mean, s, n, alpha, round_to = 4) {
    t = (sample_x_mean - u_mean) / s * sqrt(n)
    print(sprintf("t = %s", round(t, round_to)))
    if (how == "<=") {
        print(sprintf("H0: <= %s (claim)", num))
        print(sprintf("HA: > %s", num))
        if (findPUsingT("r", t, n, round_to) < alpha) {
            print("Reject H0 as well as the claim")
        } else {
            print("Fail to reject H0 as well as the claim")
        }
    } else if (how == ">=") {
        print(sprintf("H0: >= %s (claim)", num))
        print(sprintf("HA: < %s", num))
        if (findPUsingT("l", t, n, round_to) < alpha) {
            print("Reject H0 as well as the claim")
        } else {
            print("Fail to reject H0 as well as the claim")
        }
    } else if (how == "==") {
        print(sprintf("H0: == %s (claim)", num))
        print(sprintf("HA: != %s", num))
        if (findPUsingT("t", t, n, round_to) < alpha) {
            print("Reject H0 as well as the claim")
        } else {
            print("Fail to reject H0 as well as the claim")
        }
    } else if (how == "<") {
        print(sprintf("H0: >= %s", num))
        print(sprintf("HA: < %s (claim)", num))
        if (findPUsingT("l", t, n, round_to) < alpha) {
            print("Reject H0, but support the claim")
        } else {
            print("Fail to reject H0, and there is not enough evidence to support the claim.")
        }
    } else if (how == ">") {
        print(sprintf("H0: <= %s", num))
        print(sprintf("HA: > %s (claim)", num))
        if (findPUsingT("r", t, n, round_to) < alpha) {
            print("Reject H0, but support the claim")
        } else {
            print("Fail to reject H0, and there is not enough evidence to support the claim.")
        }
    } else if (how == "!=") {
        print(sprintf("H0: == %s", num))
        print(sprintf("HA: != %s (claim)", num))
        if (findPUsingT("t", t, n, round_to) < alpha) {
            print("Reject H0, but support the claim")
        } else {
            print("Fail to reject H0, and there is not enough evidence to support the claim.")
        }
    }
}