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
    def magnitude(self):
        print(self.x**2+self.y**2+self.z**2)
    def copy(self):
        return MathVector(self.x,self.y,self.z)

Q = MathVector(0,-1,2)
R = MathVector(-4,-1,-3)
P = MathVector(-1,0,-2)

PQ = Q-P
print(PQ)
PR = R-P
print(PR)
crossOfPQ_PR = PQ.crossProduct(PR)
print(crossOfPQ_PR)
crossOfPQ_PR.magnitude()