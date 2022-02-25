VIGNETTE=deposits
README=README

.PHONY: all build check doc test

all: doc build check

build: doc
	R CMD build .

#check: build
#	R CMD check typetracer*tar.gz

clean:
	-rm -f deposits*tar.gz
	-rm -fr deposits.Rcheck
	-rm -fr src/*.{o,so}

doc: clean
	Rscript -e 'devtools::document()'
	Rscript -e 'rmarkdown::render("README.Rmd")'
	Rscript -e "pkgdown::build_article('$(VIGNETTE)', quiet=FALSE)"

open:
	xdg-open docs/articles/$(VIGNETTE).html &

test:
	Rscript -e 'devtools::test()'

check:
	Rscript -e 'library(pkgcheck); checks <- pkgcheck(); print(checks); summary (checks)'

knitr: $(README).Rmd
	echo "rmarkdown::render('$(README).Rmd',output_file='$(README).md')" | R --no-save -q

install: clean
	R CMD INSTALL .
