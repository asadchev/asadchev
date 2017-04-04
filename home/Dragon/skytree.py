import Dragon

List = Dragon.List
Command = Dragon.Command
Transform = Dragon.Transform
Argument = Dragon.Argument
Var = Dragon.Var
Key = Dragon.Key

script = Dragon.Script("skytree.dvc")

script.append(List("alnum", List.alnum))
script.append(List("digits", List.digits))

class Skytree(dict): pass
skytree = Skytree()

skytree.cmd = {
    "ssh nirvana" : "S. S. H. nirvana",
    "ssh metallica" : "S. S. H. metallica",
    "ssh acdc" : "S. S. H. acdc",
    "make config-perf" : "make config perf",
    "make config-debug" : "make config debug",
    "make config-release" : "make config release",
    "make skytree-server" : "make skytree server",
    "cd skytree" : "C. D. skytree",
    "cd skytree.develop" : "C. D. skytree develop",
    }

for (k,v) in skytree.cmd.items():
    script.append(Command(v or k, k))

# for (k,v) in skytree.items():
#     voice = skytree.cmd.get(k + " ", None) or skytree.cmd.get(k,k) or k
#     script.append(List(k, v))
#     script.append(Command("%s <%s>" % (voice,k), [k + " ", Argument(0), " "]))


# script.append(List("dir", skytree.dir.values()))
# script.append(Command("<dir> dir",
# 		      Var("cmd", Argument(0)),
# 		      Transform(Var("cmd"), skytree.dir), Var("cmd")))

# script.append(List("pipe", skytree.pipe))
# script.append(Command("pipe <pipe>", ["| ", Argument(0)]))

# script.append(Command("equal quotes", ['=""', Key.left]))
# script.append(Command("dollar parens", ["$()", Key.left]))

print script
