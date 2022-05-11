
# cv – critical value / confidence interval.

findEForSampleProportion  <- function(cv, sd, n, round_to) {
   print(round(qnorm(cv + (1 - cv)/2) * sd / sqrt(n), round_to))
}

findConfidenceIntervalForSampleProportion <- function(mean, cv, sd, n, round_to) {
	print(round(mean - qnorm(cv + (1 - cv)/2) * sd / sqrt(n), round_to))
	print(round(mean + qnorm(cv + (1 - cv)/2) * sd / sqrt(n), round_to))
}

findNForSampleProportion <- function(cv, sd, err, round_to) {
	print(round((qnorm(cv + (1 - cv)/2) * sd / err)^2, round_to))
}

summaryForSampleProportion <- function (sample, cv, round_to) {
	findConfidenceIntervalForSampleProportion(mean(sample), cv, sd(sample), length(sample), round_to)
}

summaryForSampleProportionWithAssumeSD <- function (sample, cv, sd, round_to) {
	findConfidenceIntervalForSampleProportion(mean(sample), cv, sd, length(sample), round_to)
}

findConfidenceIntervalForPopulationPopulation <- function(cv, yesPopulation, totalPopulation, round_to) {
	pHat <- yesPopulation / totalPopulation
	print(round(pHat - qnorm(cv + (1 - cv)/2) * sqrt(pHat * (1- pHat) / totalPopulation), round_to))
	print(round(pHat + qnorm(cv + (1 - cv)/2) * sqrt(pHat * (1- pHat) / totalPopulation), round_to))
}

findMinimumSampleSizeWithNoPriorInformation <- function(cv, err){
	if (cv + err <= 1.01){
	print(ceiling (0.5 * 0.5 * (qnorm(cv + (1 - cv) / 2) / err) ^ 2))
	} else {
	print("cv + err > 1!")
	}
}

findMinimumSampleSizeWithPriorInformation <- function(cv, err, porb){
	if (cv + err <= 1.01){
	print(ceiling (porb * (1-porb) * (qnorm(cv + (1 - cv) / 2) / err) ^ 2))
		} else {
			print("cv + err > 1!")
	}
}
