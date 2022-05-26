import math
from typing import Union

number = Union[int, float]


class LeastSquaresRegressionFunction:
    def __init__(self, b0, b1) -> None:
        self.b0 = b0
        self.b1 = b1

    def __str__(self) -> str:
        if self.b1 > 0:
            return "y = {0} + {1}x".format(round(self.b0, 3), round(self.b1, 3))
        elif self.b1 < 0:
            return "y = {0} - {1}x".format(round(self.b0, 3), abs(round(self.b1, 3)))
        else:
            return "y = {}".format(round(self.b0, 3))

    def analyze_residual(self, x_index, real_y_value):
        predicted_value = self.b0 + x_index * self.b1
        print("Predicted value: {}".format(round(predicted_value, 3)))
        print("Residual value: {}".format(round(real_y_value - predicted_value, 5)))


def create_least_squares_regression_function(r, mean_x, mean_y, Sx, Sy):
    return LeastSquaresRegressionFunction(mean_y - Sy / Sx * r * mean_x, Sy / Sx * r)


class StatisticsToolKit:
    @staticmethod
    def separate_num(num: int, len_of_each: int = 2) -> tuple[int]:
        num_str: str = str(num)
        samples_tuple: list[int] = []
        for i in range(0, len(num_str), len_of_each):
            samples_tuple.append(int(num_str[i : i + 2]))
        samples_tuple.sort()
        return tuple(samples_tuple)

    @staticmethod
    def get_median_of_sample(samples: tuple[number]) -> number:
        samples_sorted = list(samples)
        samples_sorted.sort()
        if len(samples_sorted) % 2 == 1:
            return samples_sorted[int((len(samples_sorted) + 1) / 2) - 1]
        else:
            second_num_index: int = int(len(samples_sorted) / 2)
            return (samples_sorted[second_num_index - 1] + samples_sorted[second_num_index]) / 2

    @classmethod
    def give_five_number_summary_of_sample(cls, original_samples: tuple[number]) -> None:
        samples = list(original_samples)
        samples.sort()
        if len(samples) % 2 != 0:
            half_num = int((len(samples) - 1) / 2)
        else:
            half_num = int(len(samples) / 2)
        if half_num % 2 != 0:
            index = int((half_num + 1) / 2) - 1
            first_quartile = samples[index]
            third_quartile = samples[-index - 1]
        else:
            index = int(half_num / 2) - 1
            first_quartile = (samples[index] + samples[index + 1]) / 2
            third_quartile = (samples[-index - 1] + samples[-index - 2]) / 2

        print("Minimum: {}".format(samples[0]))
        print("First quartile: {}".format(first_quartile))
        print("Median: {}".format(cls.get_median_of_sample(original_samples)))
        print("Third quartile: {}".format(third_quartile))
        print("Maximum: {}".format(samples[-1]))
        """其他重要数据"""
        print("IQR: {}".format(third_quartile - first_quartile))
        print(
            "Range of valid sample: [{0}, {1}], as a result...".format(
                round(first_quartile - 1.5 * (third_quartile - first_quartile), 5),
                round(third_quartile + 1.5 * (third_quartile - first_quartile), 5),
            )
        )
        outliers = []
        for sample_t in samples:
            if sample_t < first_quartile - 1.5 * (third_quartile - first_quartile) or sample_t > third_quartile + 1.5 * (
                third_quartile - first_quartile
            ):
                outliers.append(sample_t)
        if len(outliers) == 0:
            print("There is no outlier in this sample.")
        else:
            print("Outliers: {}".format(outliers))

    @classmethod
    def analyze_deviation_of_sample(cls, original_samples: tuple[number]) -> None:
        samples = list(original_samples)
        samples.sort()
        print("Start analyzing sample:", samples)
        sum_of_samples = sum(samples)
        mean_of_the_sample = sum_of_samples / len(samples)
        sum_of_deviation = 0.0
        square_of_deviation = 0.0
        for each_sample in samples:
            print("deviation of {0}: {1}".format(each_sample, (each_sample - mean_of_the_sample)))
            square_of_deviation = (each_sample - mean_of_the_sample) ** 2
            print("square of deviation of {0}: {1}".format(each_sample, square_of_deviation))
            sum_of_deviation += square_of_deviation
            print("--------------------")
        print("Final result:")
        print("Num of Samples 样本数量: {}".format(len(samples)))
        print("Mean 平均: {}".format(mean_of_the_sample))
        print("Median 中位数: {}".format(cls.get_median_of_sample(samples)))
        print("sample variance: {}".format(sum_of_deviation / (len(samples) - 1)))
        print("sample standard deviation: {}".format(round(math.sqrt(sum_of_deviation / (len(samples) - 1)), 3)))

    @staticmethod
    def analyze_correlation_of_variables(
        explanatory_variables: tuple[number], response_variables: tuple[number]
    ) -> LeastSquaresRegressionFunction:
        if len(explanatory_variables) != len(response_variables):
            raise Exception("Lengths of two samples do not match")
        else:
            len_of_samples = len(explanatory_variables)
        # x轴
        sum_of_explanatory_variables = sum(explanatory_variables)
        mean_of_explanatory_variables = sum_of_explanatory_variables / len(explanatory_variables)
        # y轴
        sum_of_response_variables = sum(response_variables)
        mean_of_response_variables = sum_of_response_variables / len(response_variables)
        sum_of_deviation_xy = 0.0
        sum_of_standard_deviation_x = 0.0
        sum_of_standard_deviation_y = 0.0
        # 计算deviation
        for i in range(len(explanatory_variables)):
            deviation_x = explanatory_variables[i] - mean_of_explanatory_variables
            deviation_y = response_variables[i] - mean_of_response_variables
            sum_of_standard_deviation_x += deviation_x**2
            sum_of_standard_deviation_y += deviation_y**2
            sum_of_deviation_xy += deviation_x * deviation_y
        print(sum_of_deviation_xy)
        standard_deviation_x = math.sqrt(sum_of_standard_deviation_x / (len_of_samples - 1))
        print("Standard deviation of x: {}".format(round(standard_deviation_x, 3)))
        standard_deviation_y = math.sqrt(sum_of_standard_deviation_y / (len_of_samples - 1))
        print("Standard deviation of y: {}".format(round(standard_deviation_y, 3)))
        correlation = sum_of_deviation_xy / ((len_of_samples - 1) * standard_deviation_x * standard_deviation_y)
        print("Correlation: {}".format(round(correlation, 3)))
        b1 = correlation * standard_deviation_y / standard_deviation_x
        print("Slope: {}".format(round(b1, 3)))
        b0 = mean_of_response_variables - b1 * mean_of_explanatory_variables
        print("Y-intercept: {}".format(round(b0, 3)))
        function_of_data = LeastSquaresRegressionFunction(b0, b1)
        print("Function for the line of least squares regression: {}".format(function_of_data))
        print("R-Squared: {}".format(round(correlation**2, 3)))
        return function_of_data


Statistics = StatisticsToolKit()
