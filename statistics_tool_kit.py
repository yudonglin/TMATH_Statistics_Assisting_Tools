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
    def analyze_deviation_of_sample(self, original_samples:tuple[number]) -> None:
        samples = list(original_samples)
        samples.sort()
        print("Start analyzing sample:", samples)
        sum_of_samples = sum(samples)
        mean_of_the_sample = sum_of_samples/len(samples)
        sum_of_deviation = 0.0
        square_of_deviation = 0.0
        for each_sample in samples:
            print("deviation of {0}: {1}".format(each_sample,(each_sample-mean_of_the_sample)))
            square_of_deviation = (each_sample-mean_of_the_sample)**2
            print("square of deviation of {0}: {1}".format(each_sample,square_of_deviation))
            sum_of_deviation += square_of_deviation
            print("--------------------")
        print("Final result:")
        print("Num of Samples 样本数量: {}".format(len(samples)))
        print("Mean 平均: {}".format(mean_of_the_sample))
        print("Median 中位数: {}".format(self.get_median_of_sample(samples)))
        print("sample variance: {}".format(sum_of_deviation/(len(samples)-1)))
        print("sample standard deviation: {}".format(round(math.sqrt(sum_of_deviation/(len(samples)-1)), 3)))
    def analyze_correlation_of_variables(self, explanatory_variables:tuple[number], response_variables:tuple[number]) -> None:
        if len(explanatory_variables) != len(response_variables):
            raise Exception("Lengths of two samples do not match")
        else:
            len_of_samples = len(explanatory_variables)
        #x轴
        sum_of_explanatory_variables = sum(explanatory_variables)
        mean_of_explanatory_variables = sum_of_explanatory_variables / len(explanatory_variables)
        #y轴
        sum_of_response_variables = sum(response_variables)
        mean_of_response_variables = sum_of_response_variables / len(response_variables)
        sum_of_deviation_xy = 0.0
        sum_of_standard_deviation_x = 0.0
        sum_of_standard_deviation_y = 0.0
        #计算deviation
        for i in range(len(explanatory_variables)):
            deviation_x = explanatory_variables[i]-mean_of_explanatory_variables
            deviation_y = response_variables[i]-mean_of_response_variables
            sum_of_standard_deviation_x += deviation_x**2
            sum_of_standard_deviation_y += deviation_y**2
            sum_of_deviation_xy += deviation_x*deviation_y
        standard_deviation_x = math.sqrt(sum_of_standard_deviation_x/(len_of_samples-1))
        print("Standard deviation of x: {}".format(round(standard_deviation_x, 3)))
        standard_deviation_y = math.sqrt(sum_of_standard_deviation_y/(len_of_samples-1))
        print("Standard deviation of y: {}".format(round(standard_deviation_y, 3)))
        correlation = sum_of_deviation_xy/((len_of_samples-1)*standard_deviation_x*standard_deviation_y)
        print("Correlation: {}".format(round(correlation,3)))

Statistics = StatisticsToolKit()