all: document.pdf conclusion.pdf

%.pdf: %.tex references.bib
	pdflatex $*
	bibtex $*
	pdflatex $*
	pdflatex $*

latex: document.tex references.bib
	latex document
	bibtex document
	latex document
	latex document

clean:
	rm document.pdf

