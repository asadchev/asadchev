from Dragon import *

words = []

words.append(List("emacs-backward",
		   ( "backward",
		     "go back",
		     "move back",
		     "previous",
		     "go to previous",
		     )))

words.append(List("emacs-forward", (
	    "forward",
	    "go forward",
	    "move forward",
	    "next",
	    "go to next",
	    )))

words.append(List("emacs-delete", (
	    "delete",
	    "delete next",
	    "kill",
	    "kill next",
	    )))

words.append(List("emacs-delete-last", (
	    "backward delete",
	    "backward kill",
	    "delete last",
	    "delete previous",
	    "kill last",
	    "kill previous",
	    )))

words.append(List("emacs-capitalize",
		   (
	    "cap",
	    "cap next",
	    "capitalize",
	    "capitalize next",
	    )))

words.append(List("emacs-capitalize-last", (
	    "backward capitalize",
	    "cap last",
	    "cap previous",
	    "capitalize last",
	    "capitalize previous",
	    )))
words.append(List("emacs-upcase", (
	    "all caps",
	    "all caps next",
	    "up case",
	    "up case next",
	    )))
words.append(List("emacs-upcase-last", (
	    "all caps last",
	    "all caps previous",
	    "backward up case",
	    "up case last",
	    "up case previous",
	    )))
words.append(List("emacs-downcase", (
	    "down case",
	    "down case next",
	    "no caps",
	    "no caps next",
	    )))
words.append(List("emacs-downcase-last", (
	    "backward down case",
	    "down case last",
	    "down case previous",
	    "no caps last",
	    "no caps previous",
	    )))
words.append(List("emacs-beginning-of", (
	    "backward",
	    "beginning of",
	    "go back 1",
	    "go to beginning of",
	    "move back 1",
	    "move to beginning of",
	    )))
words.append(List("emacs-end-of", (
	    "end of",
	    "forward",
	    "go forward 1",
	    "go to end of",
	    "move forward 1",
	    "move to end of",
	    )))
words.append(List("emacs-defun", (
	    "defun",
	    "function",
	    )))
words.append(List("emacs-defuns", (
	    "defuns",
	    "functions",
	    )))
words.append(List("emacs-reverse", (
	    "backward",
	    "reverse",
	    )))
words.append(List("emacs-mark", (
	    "mark",
	    "mark next",
	    "select",
	    "select next",
	    )))
words.append(List("emacs-mark-last", (
	    "backward mark",
	    "backward select",
	    "mark last",
	    "mark previous",
	    "select last",
	    "select previous",
	    )))
words.append(List("emacs-region", (
	    "region",
	    "selection",
	    "that",
	    )))
words.append(List("emacs-expression", (
	    "expression",
	    "sexp",
	    )))
words.append(List("emacs-expressions", (
	    "expressions",
	    "sexps",
	    )))
words.append(List("emacs-spell", (
	    "ispell",
	    "spell",
	    )))
words.append(List("emacs-delete-flag", (
	    "delete",
	    "flag",
	    )))
words.append(List("emacs-evaluate", (
	    "eval",
	    "evaluate",
	    )))
words.append(List("emacs-describe", (
	    "describe",
	    "help",
	    )))

script = Script("emacs~.dvc")

script.append(List("lowercase", List.lowercase))
script.append(List("alnum", List.alnum))
script.append(List("digits", List.digits))

#script.append(List("key", key.values()))
#script.append(Command("<key> <1-50>", Loop(0, Argument(1), ["{", Argument(0), "}"])))
script.append(Command("control <lowercase> <alnum>", Key.Ctrl(Argument(0)), Argument(1)))

commands = (
    ("fill region"),
    ("fill paragraph"),
    ("next buffer"),
    ("auto fill mode"),
    ("kill whole line", "kill this line"),
    )

for c in commands:
    if not isinstance(c, str):
	for v in c: script.append(Command(v, Key.Alt("x", c[0]), Key.enter))
    else:
	script.append(Command(c, Key.Alt("x", c), Key.enter))

print script

