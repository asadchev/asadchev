
class Symmetry:
    def Integral(a,b):
	pass


class Symmetrizer(Expression):
    
    class Generator:
	def __init__(self, a, b):
	    self._indices = (a,b)
	def __getitem__(self, expression):
	    (a,b) = self._indices
	    return Symmetrizer(a, b, expression)
    def P(a,b):
	return Symmetrizer.Generator(a,b)
    P = staticmethod(P)

    def __init__(self, a, b, expression):
	self._indices = (a, b)
	self.term_ = expression
	self.permutations_ = []
	p = list(itertools.permutations(range(len(a))))
	for o in p[1:]:
	    idx = ([a[i] for i in o] + [b[i] for i in o])
	    self.permutations_.append(idx)
	Expression.__init__(self, self)	
    def __str__(self):
	a = "".join([str(i) for i in self._indices[0]])
	b = "".join([str(i) for i in self._indices[1]])
	return "P(%s/%s)[%s]" % (a, b, self.term_)
    def indices(self):
	return sum(self._indices, [])
    def __getitem__(self, expression):
	P = copy.copy(self)
	P.expression_ = expression
	return P
    def expand(self):
	e = ex = self.term_.expand()
	for p in self.permutations_:
	    d = dict([(k, v) for k,v in zip(self.indices(), p)])
	    e += ex.relabel(d)
	return e
    def __iter__(self):
	t = [ self.term_ ]
	ex = self.term_
	for p in self.permutations_:
	    d = dict([(k, v) for k,v in zip(self.indices(), p)])
	    t.append(ex.relabel(d))
	return iter(t)
