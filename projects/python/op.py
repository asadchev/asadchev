

class Sum(Expression):
    terms_ = None
    def __init__(self, a, b):
	if (a.rank() != b.rank()):
	    raise RankError
	if (sorted(a.indices()) != sorted(b.indices())):
	    raise IndexError
	self.terms_ = (a, b)
	Expression.__init__(self, self)
    def __str__(self):
	a,b = self.terms_
	if b.scale_ < 0:
	    return "%s - %s" % (a, -b)
	else:
	    return "%s + %s" % (a, b)
    def __rmul__(self, s):
	(a,b) = self.terms_
	return s*a + s*b
    def indices(self):
	return self.terms_[0].indices()
    def expand(self):
	return Sum(*map(expand, self.terms_))
    def relabel(self, indices):
	return Sum(*map(lambda t: t.relabel(indices), self.terms_))


class Contraction(Expression):
    terms_ = None
    def __init__(self, a, b):
	self.terms_ = (a,b)
	Expression.__init__(self, self, a.scale_*b.scale_)
    def __str__(self):
	a,b = self.terms_
	ab = []
	for t in (a,b):
	    fmt = "(%s)" if isinstance(t, Sum) else "%s"
	    ab.append(fmt % str(t))
	return "*".join(ab)
    def indices(self):
	indices = []
	for t in self.terms_:
	    indices = contract(t.indices(), indices)
	return indices
    def relabel(self, indices):
	return Contraction(*map(lambda t: t.relabel(indices), self.terms_))
    def expand(self):
	a,b = map(expand, self.terms_)
	s = b.scale_
	# print "Contraction %s: %s * (%s) * (%s)" % (self, s, a, b)
	if isinstance(a, Sum):
	    return Sum(*[expand((s*t)*b) for t in a.terms_])
	if isinstance(b, Sum):	    
	    return Sum(*[expand(a*(s*t)) for t in b.terms_])
	return s*(a*b)
