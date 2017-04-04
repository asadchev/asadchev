document.pdf: document.tex references.bib
	# bibtex document
	pdflatex document
	bibtex document
	pdflatex document
	pdflatex document

clean:
	rm document.pdf

zip:
	rm -f document.zip
	zip document.zip *

latex: document.tex
	latex $<
	bibtex document
