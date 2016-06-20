all : report

report : ref.bib report.tex
	pdflatex report
	bibtex report
	pdflatex report
	pdflatex report
	evince report.pdf
	
clean : 
	rm -f *.log *.aux *.dot *.gz *.dvi *.toc *.blg *.bbl *.lot *.lof *.out *~
