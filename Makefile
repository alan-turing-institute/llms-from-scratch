all: running-from-scratch.pdf

running-from-scratch.pdf: running-from-scratch.tex llms.bib
	latexmk -lualatex $<
