SCRIPTS = boost.dvc linux.dvc global.dvc my-emacs.dvc emacs~.dvc latex.dvc

all: $(SCRIPTS)
	rm -f everything.dvc
	cat *.dvc > everything.dvc

emacs~.dvc: emacs.py Dragon.dvc
	python $< > $@

%.dvc: %.py Dragon.py
	python $< > $@



