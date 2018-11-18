all: labels.pdf

labels.pdf: labels.Rmd
	Rscript -e 'library(rmarkdown); render("$<")'

labels.Rmd: make-specimen-labels.R botanical-collections_rvm.csv label-template.txt
	Rscript $<
