import math


class Person:
    def __init__(self, name, distance, initial_speed, acceleration):
        self.name = name
        self.distance = distance
        self.initial_speed = initial_speed
        self.current_speed = initial_speed
        self.acceleration = acceleration
        self.time = 0
        self.__predict()

    def __cal_speed(self):
        self.current_speed += self.acceleration

    def __predict(self):
        self.guess_time = (
            math.sqrt(self.initial_speed * self.initial_speed + 2 * self.acceleration * self.distance) - self.initial_speed
        ) / self.acceleration
        print("Computer predicts that {0} will be there in {1}s.".format(self.name, round(self.guess_time, 3)))

    def cal_position(self, t):
        return self.initial_speed * t + 0.5 * self.acceleration * t * t

    def move(self):
        self.distance -= self.current_speed
        if self.distance > 0:
            self.time += 1
            self.__cal_speed()
            return False
        else:
            return True


def compare(person1, person2):
    while True:
        MovePerson1 = person1.move()
        MovePerson2 = person2.move()
        if MovePerson1 == False and MovePerson2 == False:
            pass
        elif MovePerson1 == True and MovePerson2 == True:
            print("{0} and {1} get there almost the same time in around {2}s!".format(person1.name, person2.name, person2.time))
            name = None
            if person1.guess_time < person2.guess_time:
                print("But based on the calculation, {} should get there first.".format(person1.name))
            elif person1.guess_time > person2.guess_time:
                print("But based on the calculation, {} should get there first.".format(person2.name))
            break
        elif MovePerson1 == True:
            print("{0} get there first in around {1}s!".format(person1.name, person1.time))
            break
        elif MovePerson2 == True:
            print("{0} get there first in around {1}s!".format(person2.name, person2.time))
            break
