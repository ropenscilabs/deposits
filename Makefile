VIGNETTE=deposits
README=README

.PHONY: all build check doc test

all: docall build check

build: docall
	R CMD build .

#check: build
#	R CMD check typetracer*tar.gz

clean:
	-rm -f deposits*tar.gz
	-rm -fr deposits.Rcheck
	-rm -fr src/*.{o,so}

doc: 
	Rscript -e 'devtools::document()'

readme: 
	Rscript -e 'rmarkdown::render("$(README).Rmd")'

site: clean doc readme
	Rscript -e "pkgdown::build_home(quiet=FALSE)"
	Rscript -e "pkgdown::build_articles(quiet=FALSE)"
	Rscript -e "pkgdown::build_reference()"

docall: readme doc site

open:
	xdg-open docs/index.html &

test:
	Rscript -e 'devtools::test()'

check:
	Rscript -e 'library(pkgcheck); checks <- pkgcheck(); print(checks); summary (checks)'

knitr: $(README).Rmd
	echo "rmarkdown::render('$(README).Rmd',output_file='$(README).md')" | R --no-save -q

install: clean
	R CMD INSTALL .
