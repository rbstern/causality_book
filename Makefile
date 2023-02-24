# Makefile for knitr

# optionally put all RNW files to be compiled to pdf here, separated by spaces
RNW_FILES= $(wildcard *.Rnw)

# these pdf's will be compiled from Rnw and Rmd files
TEX_FILES= $(RNW_FILES:.Rnw=.tex) 

# these R files will be untangled from RNoWeb files
R_FILES= $(RNW_FILES:.Rnw=-purled.R) 

# cache and figure directories
CACHEDIR= cache
FIGUREDIR= figures

.PHONY: all purled clean cleanall open
.SUFFIXES: .Rnw .pdf .R .tex

# these targets will be made by default
book: $(TEX_FILES)

# use this to create R files extracted from RNoWeb files
purled: $(R_FILES)

# remove generated files except pdf and purled R files
clean:
	rm -f *.bbl *.blg *.aux *.out *.log *.spl *tikzDictionary *.fls
	rm -f $(TEX_FILES)

# run the clean target then remove pdf and purled R files too
cleanall: clean
	rm -f *.synctex.gz
	rm -f $(TEX_FILES)
	rm -f $(R_FILES)
	rm -rf ./build

copybook:
	cp build/book.tex ./


# compile a TEX from a RNoWeb file
%.tex: %.Rnw Makefile
	mkdir build	
	Rscript \
		-e "require(knitr)" \
		-e "knitr::opts_chunk[['set']](fig.path='$(FIGUREDIR)/$*-')" \
		-e "knitr::opts_chunk[['set']](cache.path='$(CACHEDIR)/$*-')" \
		-e "knitr::knit('$*.Rnw', output='build/$*.tex')"
	cp config.tex build/config.tex
	cp book.bib build/book.bib
	cp slashbox.sty build/slashbox.sty
	mkdir build/figures
	cp -a ./figures/. ./build/figures/
	
# extract an R file from an RNoWeb file
%-purled.R: %.Rnw
	Rscript \
		-e "require(knitr)" \
		-e "knitr::opts_chunk[['set']](fig.path='$(FIGUREDIR)/$*-')" \
		-e "knitr::opts_chunk[['set']](cache.path='$(CACHEDIR)/$*-')" \
		-e "knitr::purl('$*.Rnw', '$*-purled.R')"

# open all PDF's
open:
	open -a Skim $(PDFS)

