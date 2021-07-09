import math
from typing import Union

number = Union[int,float]

class StatisticsToolKit:
    def __init__(self) -> None:
        pass
    def separate_num(self, num:int, len_of_each:int=2) -> tuple[int]:
        num_str:str = str(num)
        samples_tuple:list[int] = []
        for i in range(0, len(num_str), len_of_each):
            samples_tuple.append(int(num_str[i:i+2]))
        samples_tuple.sort()
        return tuple(samples_tuple)
    def get_median_of_sample(self, samples:tuple[number]) -> number:
        samples_sorted = list(samples)
        samples_sorted.sort()
        if len(samples_sorted)%2 == 1:
            return samples_sorted[int((len(samples_sorted)+1)/2)]
        else:
            second_num_index:int = int(len(samples_sorted)/2)
            return (samples_sorted[second_num_index-1]+samples_sorted[second_num_index])/2
    def show_deviation_of_samples(self, original_samples:tuple[number]) -> None:
        samples = list(original_samples)
        samples.sort()
        print(samples)
        sum_of_samples = sum(samples)
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
        print("Final result:")
        print("Num of Samples 样本数量: {}".format(len(samples)))
        print("Mean 平均: {}".format(sum_of_samples/len(samples)))
        print("Median 中位数: {}".format(self.get_median_of_sample(samples)))
        print("sample variance: {}".format(sum_of_deviation/(len(samples)-1)))
        print("sample standard deviation: {}".format(round(math.sqrt(sum_of_deviation/(len(samples)-1)), 3)))

Statistics = StatisticsToolKit()