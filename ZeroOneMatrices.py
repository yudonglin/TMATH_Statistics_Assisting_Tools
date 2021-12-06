import numpy


class ZeroOneMatrices:
    @staticmethod
    def getBooleanProduct(m1: list, m2: list) -> numpy.ndarray:
        result = numpy.zeros((len(m1), len(m1[0])), dtype=numpy.int8)
        for row in range(len(result)):
            for col in range(len(result[row])):
                value: int = 0
                for i in range(len(m1[row])):
                    if m1[row][i] == m2[i][col] == 1:
                        value = 1
                        break
                result[row][col] = value
        return result
