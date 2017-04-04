import Dragon

List = Dragon.List
Command = Dragon.Command
Transform = Dragon.Transform
Argument = Dragon.Argument
Var = Dragon.Var
Key = Dragon.Key

script = Dragon.Script("linux.dvc")

script.append(List("alnum", List.alnum))
script.append(List("digits", List.digits))

class Linux(dict): pass
linux = Linux()

linux.cmd = { "svn " : "S. V. N.",
	      "ssh" : "S. S. H.",
	      "killall ": "",
	      "git " : "",
	      "diff " : "",
	      "make " : "",
	      "emacs" : "",
	      "apt-get " : "apt get",
	      "apt-cache " : "apt cache",
	      "tar xf " : "tar X. F.",
	      "cd " : "C. D.",
	      "cd -" : "cd minus",
	      "grep -r " : "grep R.",
	      "fg {Enter}" : "G. F.",
	      "sudo " : "",
	      "ln -s " : "L. N. S.",
	      "tail -f " : "tail F.",
	      "rm " : "R. M."
	      }

linux.dir = { "mkdir " : "make",
	      "ls " : "list",
	      "cd " : "change" }

linux["make"] = [ "clean", "install", "upload" ]

linux["svn"] = [  "add", "move", "remove",
		  "commit", "checkout", "update", "up"
		  "diff", "status", "log"
	       ]

linux["git"] = [  "clone", "branch", "commit", "add", "push", "pull", "log", "rebase", "checkout", "diff" ]

linux["apt-get"] = [ "remove", "install", "update", "upgrade" ]  

linux.pipe = [ "grep", "less", "head", "xargs", "cat", "tail", "awk" ]

for (k,v) in linux.cmd.items(): script.append(Command(v or k, k))

for (k,v) in linux.items():
    voice = linux.cmd.get(k + " ", None) or linux.cmd.get(k,k) or k
    script.append(List(k, v))
    script.append(Command("%s <%s>" % (voice,k), [k + " ", Argument(0), " "]))


script.append(List("dir", linux.dir.values()))
script.append(Command("<dir> dir",
		      Var("cmd", Argument(0)),
		      Transform(Var("cmd"), linux.dir), Var("cmd")))

script.append(List("pipe", linux.pipe))
script.append(Command("pipe <pipe>", ["| ", Argument(0)]))

script.append(Command("equal quotes", ['=""', Key.left]))
script.append(Command("dollar parens", ["$()", Key.left]))

print script
