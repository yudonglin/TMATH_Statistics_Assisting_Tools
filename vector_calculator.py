import math
class MathVector:
    def __init__(self,x,y,z):
        self.x = x
        self.y = y
        self.z = z
    def __str__(self):
        return "<{0},{1},{2}>".format(self.x,self.y,self.z)
    def __add__(self,otherVector):
        return MathVector(self.x+otherVector.x,self.y+otherVector.y,self.z+otherVector.z)
    def __sub__(self,otherVector):
        return MathVector(self.x-otherVector.x,self.y-otherVector.y,self.z-otherVector.z)
    def __mul__(self,otherVectorOrNum):
        if isinstance(otherVectorOrNum,MathVector):
            pass
        elif isinstance(otherVectorOrNum,int):
            return MathVector(self.x*otherVectorOrNum,self.y*otherVectorOrNum,self.z*otherVectorOrNum)
    def crossProduct(self,otherVector):
        return MathVector(self.y*otherVector.z-self.z*otherVector.y,-1*(self.x*otherVector.z-self.z*otherVector.x),self.x*otherVector.y-self.y*otherVector.x)
    def print_magnitude(self):
        print("√"+str(self.x**2+self.y**2+self.z**2))
    def get_magnitude(self):
        return math.sqrt(self.x**2+self.y**2+self.z**2)
    def copy(self):
        return MathVector(self.x,self.y,self.z)

class MathPoint:
    def __init__(self,x,y,z):
        self.x = x
        self.y = y
        self.z = z

def formVectorsWithTwoPoint(point1,point2):
    return MathVector(point2.x-point1.x,point2.y-point1.y,point2.z-point1.z)