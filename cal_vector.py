from vector_calculator import *

a = MathVector(-3,2,3)
b = MathVector(3,5,-5)

A = MathPoint(4,5,2)
B = MathPoint(-6,0,2)

"""
vector3 = formVectorsWithTwoPoint(MathPoint(4,5,0),MathPoint(-4,-2,3))#MathVector(4,-7,-4)
print(vector3)
vector3.print_unit_vector()
"""

a = MathVector(5,-4,4)
b = MathVector(3,-2,-4)
print(projectionOf_u_onto_v(a,b))