#!/usr/bin/python

import pdb
#pdb.set_trace()

import sympy
from sympy.core.decorators import _sympifyit, call_highest_priority


class RankError(Exception): pass
class IndexError(Exception): pass


class Index(sympy.Symbol):
    def __new__(cls, label, basis = None):
	return sympy.Symbol.__new__(cls, label)
    @property
    def label(self): return self.args[0]
    @property
    def basis(self): return self.args[1]


class TensorExpr:
    is_Tensor = True
    is_commutative = False#True
    _op_priority = 11.0
    @property
    def rank(self):
    	return len(self.indices)
    @property
    def shape(self):
    	return tuple([i.basis for i in self.indices])
    @_sympifyit('other', NotImplemented)
    @call_highest_priority('__mul')
    def __mul__(self, other):
    	return TensorMul(self, other)
    @_sympifyit('other', NotImplemented)
    @call_highest_priority('__rmul__')
    def __rmul__(self, other):
    	return TensorMul(other, self)
    @_sympifyit('other', NotImplemented)
    @call_highest_priority('__add__')
    def __add__(self, other):
    	return TensorAdd(self, other)
    def __radd__(self, other):
    	return TensorAdd(self, other)
    def __call__(self, *indices):
	rank = len(indices)
	if (rank != self.rank):
	    raise RankError("%s != %s", rank, self.rank)
	r = zip(self.indices, indices)
	return self.subs(r, simultaneous = True)
  

class TensorAdd(TensorExpr, sympy.Add):
    def __new__(cls, *terms):
	terms = map(tensorify, terms)
	indices = terms[0].indices
	rank = terms[0].rank
	for t in terms :
	    if (rank != t.rank):
		print rank, t.rank, t
		raise RankError("Rank error (%s != %s) in %s" %
				(rank, t.rank, t))
	    def pp(l): return "(%s)" % ",".join([str(i) for i in l])
	    if (sorted(indices) != sorted(t.indices)):
	    	raise IndexError("Index Error %s != %s in %s" %
	    			 (pp(indices), pp(t.indices), t))
	e = tensorify(sympy.Add.__new__(cls, *terms))
	if e.is_Mul: e = TensorMul(*e.args)
	assert isinstance(e, TensorExpr)
	return e
    @property
    def indices(self):
	return self.args[0].indices
    

class TensorMul(TensorExpr, sympy.Mul):
    def __new__(cls, *terms):
	terms = map(tensorify, terms)
	e = tensorify(sympy.Mul.__new__(cls, *terms))
	assert isinstance(e, TensorExpr)
	return e
    @property
    def indices(self):
	t = [a for a in self.args if isinstance(a, TensorExpr)]
	return contract(*[a.indices for a in t])[0]
    # need to handle internal index renaming here
    def subs(self, *args, **kw):
	print args
	r = dict(*args)
	
	return sympy.Mul.subs(self, r, **kw)


class Tensor(TensorExpr, sympy.Expr):
    def __new__(cls, base, indices):
	return sympy.Expr.__new__(cls, base, sympy.Tuple(*indices))
    @property
    def free_symbols(self): return [self]
    @property
    def base(self):
	return self.args[0]
    @property
    def indices(self):
	return self.args[1]
    def _sympystr(self, p):
    	indices = map(p.doprint, self.indices)
    	return "%s(%s)" % (p.doprint(self.base), ",".join(indices))
    def _hashable_content(self):
     	return (self.base, self.indices)


class TensorSymbol(sympy.Atom):
    def __new__(cls, name):
    	return sympy.Basic.__new__(cls, sympy.sympify(name))
    def __str__(self):
    	return str(self.args[0])
    def _sympystr(self, p):
	return "%s" % (p.doprint(self.args[0]))
    def __call__(self, *indices):
	return Tensor(self, sympy.Tuple(*[i for i in indices]))



def indices(labels, basis = None):
    return list([Index(l, basis) for l in labels])

def symbols(labels):
    return list([TensorSymbol(l) for l in labels])

def contract(*terms):
    up, dn = [], []
    for t in terms:
	up += t[:len(t)/2]
	dn += t[len(t)/2:]
    free = [i for i in up+dn if not (i in dn and i in up)]
    dummy = [i for i in up if i in dn]
    return free,dummy
    #return sum(contract2(up, dn), [])

def contract2(up,dn):
    if up == dn:
	return [],[]
    intersection = [i for i in up if i in dn]
    if not intersection:
	return up, dn
    v = intersection[0]
    idx = up.index(v)
    print up,dn,idx
    up.remove(v)
    dn[dn.index(v)] = dn.pop(idx)
    return contract2(up,dn)


def tensor_symbols(expr):
    #print expr.free_symbols
    #print expr, type(expr), map(type, expr.free_symbols)
    return [s for s in expr.free_symbols if isinstance(s, TensorExpr)]
    
def tensorify(expr):
   # print type(expr), expr, expr.args, tensor_symbols(expr)
    if len(tensor_symbols(expr))==0: # No tensor symbols present
	return expr
    #print expr
    class_dict = { sympy.Mul:TensorMul, TensorMul:TensorMul,
		   sympy.Add:TensorAdd, TensorAdd:TensorAdd}
    if expr.__class__ not in class_dict.keys():
	return expr
    args = map(tensorify, expr.args) # Recursively call down the tree
    e = sympy.Expr.__new__(class_dict[expr.__class__], *args)
    return e

def simplify(expr):
    return tensorify(sympy.simplify(expr))

def expand(expr):
    return tensorify(sympy.expand(expr))



class Symmetrizer(TensorExpr, sympy.Expr):
    
    class Generator:
	def __init__(self, a, b):
	    self._indices = (a,b)
	def __getitem__(self, expression):
	    (a,b) = self._indices
	    return Symmetrizer(sympy.Tuple(*a), sympy.Tuple(*b), expression)
    @classmethod
    def P(cls,a,b):
	return Symmetrizer.Generator(a,b)

    @property
    def indices(self):
	return self.args[2].indices
    def _sympystr(self, p):
    	a = "".join([str(i) for i in self.args[0]])
    	b = "".join([str(i) for i in self.args[1]])
    	return "P(%s/%s)[%s]" % (a, b, self.args[2])
    def expand(self, *args, **kw):
	a,b,expr = self.args
	assert len(a) == len(b)
	terms = []
	for o in itertools.permutations(range(len(a))):
	     p = [a[i] for i in o] + [b[i] for i in o]
	     r = dict([(k, v) for k,v in zip(a+b, p)])
	     e = expr.subs(r, simultaneous = True)
	     terms.append(e)
	return TensorAdd(*terms).expand(*args, **kw)
	

P = Symmetrizer.P


# these are tensor indices in some basis
p,q,r,s = indices("pqrs", "N")
i,j,k,l,m,n = indices("ijklmn", "O")
a,b,c,d,e,f,g = indices("abcdefg", "V")

# these are tensor symbols
v,t,I = symbols("vtI")

# # some tensor expression
# x = (t(a)*t(b)*t(i,j) + 2*v(b,a)*t(i,j) + v(a,b)*t(i,j))
# print x

# # no explanation needed
# print simplify(x)
# print expand(x)

# # symmetrize wrt ij/ab
# x = P([i,j], [a,b])[ x ]
# print x
# print sympy.simplify(x)
# print sympy.expand(x)

# # relabel expression
# # this one is kinda dumb now as it doesn't relabel internal indices
# print x(k,l,e,f)

# # CR-CC
# z = t(i,a)*v(j,k,b,c) + t(j,b)*v(i,k,a,c) + t(k,c)*v(i,j,a,b)
# bar = lambda x: (Four/Three)*x(i,j,k,a,b,c) - Two*x(i,j,k,a,c,b) + (Two/Three)*x(i,j,k,b,c,a)

I = t(i,e)*t(e,i)#*t(j,b)
print I, I.indices
# fails - fix dummy subs
x = (v(a,b,e,m,k,l)*t(m,e)).subs({a:e})
print x, x.indices
#M = t(i,j,a,e)*I(k,e,c,b)
