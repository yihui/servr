all: example.html

%.html: %.Rmd
	Rscript -e "rmarkdown::render('$^')"
