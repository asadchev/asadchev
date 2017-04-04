document.pdf: document.tex
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
