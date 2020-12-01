from vector_calculator import MathPoint, MathVector, formVectorsWithTwoPoint
"""
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
a = MathVector(-8,3,-2)
b = MathVector(-1,7,3 )
print(a.crossProduct(b))
"""
P = MathPoint(3,3,-3)
Q = MathPoint(-1,3,-1)
R = MathPoint(5,-3,0)


RP = formVectorsWithTwoPoint(R,P)
RQ = formVectorsWithTwoPoint(R,Q)

v = RP.crossProduct(RQ)

print(v.get_magnitude())

