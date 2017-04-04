import Dragon

List = Dragon.List
Command = Dragon.Command
Transform = Dragon.Transform
Argument = Dragon.Argument
Var = Dragon.Var
Key = Dragon.Key

script = Dragon.Script("latex.dvc")

script.append(List("alnum", List.alnum))
script.append(List("digits", List.digits))

class Latex(dict): pass
latex = Latex()

latex.cmd = { "cite" : "",
	      "begin" : "",
	      "end" : "",
	      "item" : "",
	      "ref" : ""
	      }

for (k,v) in latex.cmd.items():
    voice = latex.cmd.get(k + " ", None) or latex.cmd.get(k,k) or k
    script.append(List(k, v))
    script.append(Command("backslash %s" % (voice),
			  ["\\", k, "{}", Key.left]))

print script
