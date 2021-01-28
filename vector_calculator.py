import math

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
    def cross(self,otherVector):
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

def dotProduct(v1:MathVector,v2:MathVector) -> float: return v1.dot(v2)

def crossProduct(v1:MathVector,v2:MathVector) -> MathVector: return v1.cross(v2)

def formVectorsWithTwoPoint(point1,point2) -> MathVector:
    return MathVector(point2.x-point1.x,point2.y-point1.y,point2.z-point1.z)

def workDoneBy(force:MathVector,distance:MathVector) -> float: return force.dot(distance)

def findAreaOfTriangleFormBy(P:MathPoint,Q:MathPoint,R:MathPoint) -> float:
    PQ = formVectorsWithTwoPoint(P,Q)
    PR = formVectorsWithTwoPoint(P,R)
    PQR = PQ.cross(PR)
    return math.sqrt(PQR.magnitude_sqr/4)

def findVolumeOfParallelepipedFormBy(a:MathVector,b:MathVector,c:MathVector):
    return abs(a.dot(b.cross(c)))

def angleBetween(v1,v2,inDegree=False) -> float:
    if inDegree:
        return math.acos(abs(v1.dot(v2))/math.sqrt(v1.magnitude_sqr*v2.magnitude_sqr))/math.pi*180
    else:
        return math.acos(abs(v1.dot(v2))/math.sqrt(v1.magnitude_sqr*v2.magnitude_sqr))

def find_equation_of_plain(a_vector:MathVector,a_point:MathPoint) -> None:
    #x值
    if a_vector.x == 1:
        x_value = "x"
    elif a_vector.x == -1:
        x_value = "-x"
    elif a_vector.x == 0:
        x_value = ""
    else:
        x_value = "{}x".format(a_vector.x)
    #y值
    if a_vector.y == 1:
        y_value = "+y"
    elif a_vector.y == -1:
        y_value = "-y"
    elif a_vector.y == 0:
        y_value = ""
    elif a_vector.y > 1:
        y_value = "+{}y".format(a_vector.y)
    else:
        y_value = "{}y".format(a_vector.y)
    #z值
    if a_vector.z == 1:
        z_value = "+z"
    elif a_vector.z == -1:
        z_value = "-z"
    elif a_vector.z == 0:
        z_value = ""
    elif a_vector.z > 1:
        z_value = "+{}z".format(a_vector.z)
    else:
        z_value = "{}z".format(a_vector.z)

    print("{0}{1}{2} = {3}".format(
        x_value,
        y_value,
        z_value,
        a_vector.x*a_point.x + a_vector.y*a_point.y + a_vector.z*a_point.z)
    )

def projectionOf_u_onto_v(v:MathVector,u:MathVector) -> MathVector: return v*(dotProduct(u,v)/v.magnitude_sqr)
