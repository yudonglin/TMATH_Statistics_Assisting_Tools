from vector_calculator import *

a = MathVector(-3, 2, 3)
b = MathVector(3, 5, -5)

A = RectangularCoordinates(4, 5, 2)
B = RectangularCoordinates(-6, 0, 2)

"""
vector3 = formVectorsWithTwoPoint(RectangularCoordinates(4,5,0),RectangularCoordinates(-4,-2,3))#MathVector(4,-7,-4)
print(vector3)
vector3.print_unit_vector()
"""

a = MathVector(5, -4, 4)
b = MathVector(3, -2, -4)
print(vector_projection_of_u_onto_v(a, b))

# distance1
pq = formVectorsWithTwoPoint(
    RectangularCoordinates(8, -9, 3), RectangularCoordinates(0, -8, -9)
)
v = MathVector(-1, 3, 4)
crossTmp = crossProduct(pq, v)
print(abs(crossTmp.magnitude) / abs(v.magnitude))
# distance2
v = MathVector(2, 0, 5)
p = RectangularCoordinates(-0.5, 0, 0)
q = RectangularCoordinates(0, 0, 0.2)
pq = formVectorsWithTwoPoint(p, q)
dotTmp = dotProduct(pq, v)
print(abs(dotTmp) / abs(v.magnitude))

# Find the equation of a plane through the points
a = RectangularCoordinates(-1, 3, -4)
b = RectangularCoordinates(5, -1, 6)
c = RectangularCoordinates(1, 8, -2)
ab = formVectorsWithTwoPoint(a, b)
ac = formVectorsWithTwoPoint(a, c)
abc = crossProduct(ab, ac)
find_equation_of_plain(abc, a)
