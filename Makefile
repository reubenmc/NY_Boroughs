all: hw3.html

hw3.html: hw3.Rmd cleanData.R Reuben_Q2.R visualization.R
	Rscript -e "library(rmarkdown);render('hw3.Rmd')"

nyBoroughs.Rdata: cleanData.R
	R --no-save < cleanData.R

boroughs.json: nyBoroughs.Rdata Reuben_Q2.R
	R --no-save < Reuben_Q2.R

clean:
	rm -rf nyBoroughs.Rdata
	rm -f hw3.html

.PHONY: all clean



