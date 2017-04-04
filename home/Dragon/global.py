from Dragon import *

script = Script("global.dvc")

script.append(List("alnum", List.alnum))
script.append(List("digit", List.digits))
script.append(List("1-50", [str(i) for i in range(1, 51)]))
script.append(List("character", List.alnum + "/"))

heard = { "open commands" : "open command browser",
	  "open words" : "open vocabulary editor",
	  "firefox" : "switch to Firefox"
	  }

for (k,v) in heard.items():
    script.append(Command(k, HeardWord(v)))


cmd = { Key("enter") : "enter",
	Key("escape") : "escape",
	Key("tab") : "tab",
	Key("backspace") : "backspace",
	", " : "comma space" }


key = { Key.enter : "enter",
	Key.escape : "escape",
	Key.tab : "tab",
	Key.delete : "delete",
	Key.space : "space",
	Key.right : "right",
	Key.left : "left",
	Key.up : "up",
	Key.down : "down",
	Key.delete : "delete",
	Key.backspace : "backspace"
	}

symbols = { "." : "dot",
	    "." : "point",
	    "^" : "caret",
	   "/" : "slash",
	   "$" : "dollar",
	   "%" : "percent",
	   "*" : "star",
	   "&" : "ampersand",
	   "\\" : "backslash",
	   "#" : "pound",
	   "," : "comma",
	    "-" : "hyphen",
	    "_" : "underscore",
	    "~": "tilde"
	   }

script.append(List("special-symbol", symbols.values()))
script.append(Command("<special-symbol>",
		      Var("cmd", Argument(0)),
		      Transform(Var("cmd"), symbols), Var("cmd")))
script.append(Command("<special-symbol> <alnum>",
		      Var("cmd", Argument(0)),
		      Transform(Var("cmd"), symbols),
		      [Var("cmd"), Argument(1)]))
script.append(Command("<alnum> <special-symbol>",
		      Var("cmd", Argument(1)),
		      Transform(Var("cmd"), symbols),
		      [Argument(0), Var("cmd")]))
script.append(Command("<special-symbol> <special-symbol>",
		      Var("s1", Argument(0)),
		      Var("s2", Argument(1)),
		      Transform(Var("s1"), symbols),
		      Transform(Var("s2"), symbols),
		      [Var("s1"), Var("s2")]))

script.append(Command("brakets <alnum>", [ "[", Argument(0), "]"]))
script.append(Command("braces <alnum>", [ "{{", Argument(0), "}}"]))
script.append(Command("parens <alnum>", [ "(", Argument(0), ")"]))
script.append(Command("quotes <alnum>", [ "'", Argument(0), "'"]))

script.append(List("number", ["number", "num", "choose"]))
script.append(Command("<number> <digit>", [Argument(1)])) 
script.append(Command("<number> <digit> <digit>",
		      [Argument(1), Argument(2)])) 
script.append(Command("<number> <digit> <digit> <digit>",
		      [Argument(1), Argument(2), Argument(3)])) 


script.append(Command("<alnum> <alnum>", [ Argument(0), Argument(1) ] ))
script.append(Command("<alnum> <alnum> <alnum>",
		      [ Argument(0), Argument(1), Argument(2) ] ))

script.append(List("alt", [ "alt", "meta" ]))
script.append(Command("<alt> tab", Key.Alt("Tab")))
script.append(Command("<alt> <character>", Key.Alt(Argument(1)) ))
script.append(Command("control <character>",
		      Key.Ctrl(Argument(0)), HeardWord("\No-Caps") ))

script.append(List("key", key.values()))
script.append(Command("<key> <1-50>", Loop(0, Argument(1), ["{", Argument(0), "}"])))
script.append(Command("<1-50> <key>", Loop(0, Argument(0), ["{", Argument(1), "}"])))

script.append(List("global-cmd", [v or k for (k,v) in cmd.items()]))
script.append(Command("<global-cmd>",
		      Var("cmd", Argument(0)),
		      Transform(Var("cmd"), cmd), Var("cmd")))


script.append(List("lowercase", List.lowercase))
script.append(Command("cap <lowercase>", Function.UCase(Argument(0))))


print script
