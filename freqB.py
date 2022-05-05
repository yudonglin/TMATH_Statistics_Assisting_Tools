import math


def makeFreqTable(_sample: tuple, numberOfClasses: int) -> None:
    minOfSample: int = min(_sample)
    width = math.ceil((max(_sample) - minOfSample) / numberOfClasses)
    for i in range(numberOfClasses):
        lower_end: int = minOfSample + width * i
        upper_end: int = minOfSample + width * (i + 1) - 1
        freq: int = 0
        for num in _sample:
            if lower_end <= num <= upper_end:
                freq += 1
        print("class:", i + 1, "range:", lower_end, "-", upper_end, "frequency:", freq)
