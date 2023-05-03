LFILE = deposits

all: fig1
#all: knith open 

knith: $(LFILE).Rmd
	echo "rmarkdown::render('$(LFILE).Rmd',output_file='$(LFILE).html')" | R --no-save -q

knitr: $(LFILE).Rmd
	echo "rmarkdown::render('$(LFILE).Rmd',rmarkdown::md_document(variant='gfm'))" | R --no-save -q

open: $(LFILE).html
	xdg-open $(LFILE).html &

clean:
	rm -rf *.html *.png 



fig1: pdflone convone rmone

pdflone: fig1.tex
	pdflatex fig1.tex

convone: fig1.pdf
	convert -density 300 fig1.pdf -quality 100 fig1.png

rmone:
	rm fig1.aux fig1.log fig1.pdf
