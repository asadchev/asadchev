import math

print "hello"

def distance(r1, r2 = (0,0,0)):
    return math.sqrt(sum([(r1[i] - r2[i])**2 for i in range(3)]))

def force(a, b):
    return (a.Z*b.Z)/distance(a, b)

class atom(list):
    def __init__(self, r = (0,0,0), Z = 0):
	self.extend(r)
	self.Z = Z


print atom()

print distance(atom([9,5,2]))

print force(atom([9,5,2], 1), atom([9,5,-2], 1))

A = [ atom([9,5,2], 1),
      atom([9,1,2], 1),
      atom([2,-7,2], 4) ]

E = 0
for j in range(len(A)):
    for i in range(j):
	E += force(A[i], A[j])

print E
