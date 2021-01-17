import math
import fractions

class MathPoint:
    def __init__(self,x,y,z=0):
        self.x = x
        self.y = y
        self.z = z
    def __str__(self):
        return "<{0},{1},{2}>".format(self.x,self.y,self.z)

def _coordWithBracket(coord):
    return "{"+str(coord)+"}"

class MathVector(MathPoint):
    def __init__(self,x:float,y:float,z:float=0):
        MathPoint.__init__(self,x,y,z)
    def __add__(self,otherVector):
        return MathVector(
            self.x + otherVector.x,
            self.y + otherVector.y,
            self.z + otherVector.z
        )
    def __sub__(self,otherVector):
        return MathVector(
            self.x - otherVector.x,
            self.y - otherVector.y,
            self.z - otherVector.z
        )
    def __mul__(self,num:int):
        return MathVector(
            self.x * num,
            self.y * num,
            self.z * num
        )
    def dot(self,otherVector):
        return self.x*otherVector.x+self.y*otherVector.y+self.z*otherVector.z
    def crossProduct(self,otherVector):
        return MathVector(
            self.y*otherVector.z-self.z*otherVector.y,
            -1*(self.x*otherVector.z-self.z*otherVector.x),
            self.x*otherVector.y-self.y*otherVector.x
        )
    def print_magnitude(self):
        print("√"+str(self.x**2+self.y**2+self.z**2))
    def print_unit_vector(self):
        print("<\\frac{1}{0}, \\frac{2}{0}, \\frac{3}{0}>".format(self.magnitude_format
        ,_coordWithBracket(self.x),_coordWithBracket(self.y),_coordWithBracket(self.z)))
    @property
    def magnitude(self):
        return math.sqrt(self.x**2+self.y**2+self.z**2)
    @property
    def magnitude_sqr(self):
        return self.x**2+self.y**2+self.z**2
    @property
    def magnitude_format(self):
        return "{\sqrt{"+str(self.magnitude_sqr)+"}}"
    def copy(self):
        return MathVector(self.x,self.y,self.z)

def dotProduct(v1,v2) -> MathVector:
    return v1.dot(v2)

def formVectorsWithTwoPoint(point1,point2) -> MathVector:
    return MathVector(point2.x-point1.x,point2.y-point1.y,point2.z-point1.z)

def workDoneBy(force,distance) -> float:
    return force.dot(distance)

def angleBetween(v1,v2,inDegree=False) -> float:
    if inDegree:
        return math.acos(v1.dot(v2)/v1.magnitude*v2.magnitude)/math.pi*360
    else:
        return math.acos(v1.dot(v2)/v1.magnitude*v2.magnitude)

def projectionOf_u_onto_v(v,u):
    return v*(dotProduct(u,v)/v.magnitude_sqr)
