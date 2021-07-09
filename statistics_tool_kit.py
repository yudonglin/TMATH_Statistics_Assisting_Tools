import math

class StatisticsToolKit:
    def __init__(self) -> None:
        pass
    def show_deviation_of_samples(self, samples:tuple[int]) -> None:
        sum_of_samples = sum(samples)
        print("Mean: {}".format(sum_of_samples/len(samples)))
        sum_of_deviation:float = 0.0
        square_of_deviation:float = 0.0
        for each_sample in samples:
            print(
                "deviation of {0}: {1}"
                .format(
                    each_sample,
                    (each_sample-sum_of_samples/len(samples))
                    )
                )
            square_of_deviation = (each_sample-sum_of_samples/len(samples))**2
            print(
                "square of deviation of {0}: {1}"
                .format(
                    each_sample,
                    square_of_deviation
                    )
                )
            sum_of_deviation += square_of_deviation
            print("--------------------")
        print("sample variance: {}".format(sum_of_deviation/(len(samples)-1)))
        print("sample standard deviation: {}".format(round(math.sqrt(sum_of_deviation/(len(samples)-1)), 3)))

Statistics = StatisticsToolKit()