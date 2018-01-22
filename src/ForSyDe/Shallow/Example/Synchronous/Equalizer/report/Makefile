MAIN=forsyde-shallow-equalizer
OPEN=evince

FIGS_ORIG_DIR=figs
FIGS_DIR=figs

# Figures targets
XFIG_FIGS       = $(patsubst $(FIGS_ORIG_DIR)/%.fig,$(FIGS_DIR)/%.pdf_t,$(wildcard $(FIGS_ORIG_DIR)/*.fig))
EPS_FIGS	= $(patsubst $(FIGS_ORIG_DIR)/%.eps,$(FIGS_DIR)/%.pdf,$(wildcard $(FIGS_ORIG_DIR)/*.eps))

# Source files
BIBFILES	:= $(shell find . -name '*.bib')
TEXFILES	:= $(shell find . -name '*.tex')
STYLEFILES	:= $(shell find . -name '*.sty')


# Targets
all: pdf
pdf: $(MAIN).pdf


view: pdf
	@$(OPEN) $(MAIN).pdf

$(MAIN).pdf : $(MAIN).tex figures  $(TEXFILES) \
              Makefile $(STYLEFILES) $(BIBFILES)
	pdflatex --shell-escape $(MAIN)
	biber $(MAIN)
	pdflatex $(MAIN)
	@while ( grep "Rerun to get cross-references" 	\
	$(MAIN).log > /dev/null ); do		\
	        echo '** Re-running LaTeX **';		\
	        pdflatex $(MAIN);				\
	done

save-fig:
	for f in Equalizer-Level1.fig AudioFilter.fig FSM_LevelControl.fig ButtonControl.fig AudioAnalyzer.fig DistortionControl.eps; do \
		cp figs/$$f figs.bak/; \
	done

clean:
	rm -f $(XFIG_FIGS)
	rm -f figs/*.pdf
	rm -f *.blg *.aux *.log *~ *.bbl *.pdf *.bcf *.run.xml

figures: $(XFIG_FIGS)


### Rules for figures
### These are Pattern Rules and will match in the
### order they appear in the Makefile

# Rule for SVG files: use inkscape to convert to pdf
figs/%.pdf: $(S_FIGS_DIR)/%.svg
	@$(MKDIR) $(@D)
	@$(INKSCAPE) -z -D --export-pdf=$@ $(<)

# Rules for FIG files (xfig)
# Create combined pdf/latex figures from .fig file
$(XFIG_FIGS): figs/%.pdf_t: figs/%.fig
	@echo $*
	fig2dev -L pdftex -p dummy figs/$*.fig > figs/$*.pdf
	fig2dev -L pdftex_t -p figs/$* figs/$*.fig > figs/$*.pdf_t 

# Rules for original EPS figures
GS_OPTS:= -dPDFX
$(EPS_FIGS) : figs/%.pdf : figs_orig/%.eps
        #Creates .pdf files from .esp files
	a2ping --gsextra='$(GS_OPTS)' --outfile=$@  $(<)

.PHONY: all clean pdf figures

# Include auto-generated dependencies
-include *.d
