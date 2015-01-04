all: example.html

%.html: %.Rmd
	Rscript -e "knitr::knit2html('$^')"
