import string


class Argument:
    def __init__(self, value):
	self.value = value+1
    def __str__(self):
	return "_arg%i" % self.value

class Var:
    def __init__(self, name, value = None):
	self.name = name
	self.value = value
    def __str__(self):
	if self.value is None: return self.name
	return self.name + " = " + quote(self.value)

class Function:
    def __init__(self, f, *args):
	self.value = f + "(%s)" % ",".join([quote(a) for a in args])
    def __str__(self): return self.value
    def UCase(arg): return Function("UCase$", arg)
    UCase = staticmethod(UCase)
    def Str(arg): return Function("Str$", arg)
    Str = staticmethod(Str)
    # def Replace(arg, f, r): return Function("Replace", arg, f, r)
    # Replace = staticmethod(Replace)

def quote(a):
    if isinstance(a, (Argument,Key,Var,Function)):
	return str(a)
    return "\"%s\"" % a 


class Key:
    def __init__(self, keys, *cmd):
	self.value = "\"{%s}\"" % keys
	if cmd: self.value += " + %s" % (" + ".join(quote(i) for i in cmd))
    def __str__(self): return self.value

    def make(keys, cmd):
	value = ""
	q = quote(keys[1])
	if q != str(keys[1]): value = "%s+%s" % (keys[0], keys[1])
	else: value = "%s+\" + %s + \"" % (keys[0], keys[1])
	return Key(value, *cmd)
    make = staticmethod(make)

    def Alt(key, *cmd): return Key.make(["Alt", key], cmd)
    Alt = staticmethod(Alt)
    #Meta = staticmethod(Alt)

    def Ctrl(key, *cmd): return Key.make(["Ctrl", key], cmd)
    Ctrl = staticmethod(Ctrl)

Key.left = Key("Left")
Key.right = Key("Right")
Key.enter = Key("Enter")
Key.escape = Key("Escape")
Key.tab = Key("Tab")
Key.delete = Key("Delete")
Key.space = Key("Space")
Key.right = Key("Right")
Key.left = Key("Left")
Key.up = Key("Up")
Key.down = Key("Down")
Key.backspace = Key("Backspace")
Key.delete = Key("Delete")


class Emacs:
    def numeric(argument): return Key.Ctrl('u', argument)
    numeric = staticmethod(numeric)

class Transform(list):
    def __init__(self, argument, transform):
	self.transform = transform
	format = "if %s = \"%%s\" then %s = %%s" % (argument, argument)
	for (k,v) in transform.items():
	    if v: self.append(format % (v,quote(k)))

class HeardWord(list):
    def __init__(self, phrase):
	self.append("HeardWord " + ", ".join(["\"%s\"" % s for s in phrase.split()]))


class SendKeys:
    def __init__(self, items):
	self.cmd = " + ".join([quote(i) for i in items])
    def __str__(self):
	return "SendKeys %s" % self.cmd


class Loop(list):
    def __init__(self, begin, end, *items):
	self.append("I = %s" % begin)
	self.append("DO UNTIL I = %s" % end)
	self.extend(Command("", *items))
	self.append("I = I+1")
	self.append("LOOP")


class Command(list):
    def __init__(self, name, *items):
	self.name = name
	for i in items:
	    if isinstance(i, Var) and i.value:
		self.append(str(i))
	    elif isinstance(i, (Transform,Loop,HeardWord)):
		self.extend(i)
	    else:
		if type(i) is list: i = tuple(i)
		if type(i) is not tuple: i = tuple([i])
		self.append(SendKeys(i))

    def __str__(self):
	keys = "\n".join(["        %s" % i for i in self])
	return ("COMMAND \"%s\" {\n" % self.name + 
		"    SCRIPT {\n" + 
		keys +
		"\n" +
		"    }\n" + 
		"}\n")


class List(list):
    lowercase = string.ascii_lowercase
    alnum = (string.ascii_lowercase + string.digits)
    digits = (string.digits)
    def __init__(self, name, items):
	self.extend(items)
	self.name = name
    def __str__(self):
	return ("LIST \"%s\" {\n" % self.name +
		"\n".join("    \"%s\"" % i for i in self) +
		"\n}")


class Script(list):
    def __init__(self, name, items = ()):
	self.name = name
	self.extend(items)

    def __str__(self):
	return (
	    "MENU \"%s\" {\n" % self.name +
	    "STATE \"\" {\n" +
	    "\n" +
	    "\n\n".join([str(i) for i in self]) +
	    "\n\n" +
	    "}\n" +
	    "}\n")

