BIB_LTX = '\\bibliography\{'
CITE_LTX = 'LaTeX Warning: Citation'
WARN_LTX = 'Rerun to get cross-references right|There were undefined references|There were undefined citations'

.PRECIOUS: Manual.dvi Manual.ps Mistakes.dvi Mistakes.ps

all:	Manual.pdf Mistakes.pdf

%.pdf: %.ps
	ps2pdf $< $@

%.ps: %.dvi
	dvips $< -o $@

%.dvi: %.tex
	latex $<
	if egrep -s $(BIB_LTX) $<; then \
	  bibtex $(basename $<) || { rm -f $@; exit 1; }; \
	  latex $<; \
	  if egrep -s $(CITE_LTX) $*.log; then \
	    bibtex $(basename $<) || { rm -f $@; exit 1; } \
	  fi; \
	  latex $<; \
	fi
	while egrep -s $(WARN_LTX) $*.log; do \
	  latex $< || { rm -f $@; exit 1; } \
	done

clean:
	rm -f *.aux *.bbl *.blg *.dvi *.log *.ps *.out
