 document.pdf: document.tex
	pdflatex document
	bibtex document
	pdflatex document
	pdflatex document

latex: document.tex
	bibtex document
	latex document
	latex document

clean:
	rm document.pdf

