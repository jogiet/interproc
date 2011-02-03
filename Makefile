include Makefile.config

#---------------------------------------
# Directories
#---------------------------------------

PREFIX = $(INTERPROC_PREFIX)

# For the WEB version
OCAMLHTML_INSTALL = $(HOME)/pkg/ocamlhtml/$(ARCH)

REQ_PKG = "camllib fixpoint gmp apron \
apron.boxMPQ apron.octMPQ apron.polkaMPQ apron.t1pMPQ apron.ppl apron.polkaGrid"
REQ_PKGf = "camllib fixpoint gmp apron \
apron.boxD apron.octD apron.polkaMPQ apron.t1pD apron.ppl apron.polkaGrid"

OCAMLINC = -I $(OCAMLHTML_INSTALL)/lib

MLMODULES = spl_syn pSpl_syn spl_yacc spl_lex boolexpr equation syn2equation option solving solvingPolicy frontend

#---------------------------------------
# Rules
#---------------------------------------

# Global rules
all: interproc.byte interproc.opt interprocf.opt

interproc.byte: $(MLMODULES:%=%.cmo) interproc.cmo 
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) $(OCAMLINC) -package $(REQ_PKG) -linkpkg \
	-o $@ $^
interproc.opt: $(MLMODULES:%=%.cmx) interproc.cmx
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(REQ_PKG) -linkpkg \
	-o $@ $^

interprocf.opt: $(MLMODULES:%=%.cmx) interproc.cmx
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(REQ_PKGf) -linkpkg \
	-o $@ $^

install:
	$(INSTALLd) $(PREFIX)/bin
	for i in interproc.byte interproc.opt interprocf.opt; do \
		$(INSTALL) $$i $(PREFIX)/bin; \
	done

uninstall:
	for i in interproc.byte interproc.opt interprocf.opt; do \
		/bin/rm -f $(PREFIX)/bin/$$i; \
	done

interprocweb.cgi: $(MLMODULES:%=%.cmx) interprocweb.cmx mainweb.cmx
	$(OCAMLFIND) ocamlopt -o $@ -verbose $(OCAMLINC) -ccopt "-static"  \
	-package $(REQ_PKG) -linkpkg html.cmxa $^
interprocwebf.cgi: $(MLMODULES:%=%.cmx) interprocweb.cmx mainweb.cmx
	$(OCAMLFIND) ocamlopt -o $@ -verbose $(OCAMLINC) -ccopt "-static"  \
	-package $(REQ_PKGf) -linkpkg html.cmxa $^

distclean: clean
	$(RM) -r *.pdf html Makefile.depend

clean:
	$(RM) *.[aoc] *.cm[ioxa] *.annot spl_lex.ml spl_yacc.ml spl_yacc.mli interproc.byte interproc.opt interprocf.opt interprocweb interprocweb.cgi *~ *.idx *.ilg *.ind *.log *.toc *.dvi *.out *.aux *.bbl *.blg *.makeimage *.html *.png *.ps ocamldoc.* *.output

#---------------------------------------
# TEX and HTML rules
#---------------------------------------

.PHONY: html interproc.dvi manual.dvi

manual.pdf: manual.dvi
	$(DVIPDF) manual.dvi

manual.dvi:
	$(LATEX) manual
	bibtex manual
	$(LATEX) manual
	$(LATEX) manual

interproc.pdf: interproc.dvi
	$(DVIPDF) interproc.dvi

interproc.dvi: $(MLINT) $(MLSRC)
	$(OCAMLDOC) $(OCAMLINC) \
	-latextitle 1,chapter -latextitle 2,section -latextitle 3,subsection -latextitle 4,subsubsection -latextitle 5,paragraph -latextitle 6,subparagraph -noheader -notrailer -latex -o ocamldoc.tex $(MLMODULES:%=%.mli) interproc.mli
	$(LATEX) interproc
	$(LATEX) interproc
	$(LATEX) interproc

html: $(MLINT) $(MLSRC)
	mkdir -p html
	$(OCAMLDOC) $(OCAMLINC) -html -d html -colorize-code $(MLMODULES:%=%.mli)

homepage: html interproc.pdf manual.pdf
	hyperlatex -gif manual
	hyperlatex manual	
	cp -r html interproc.pdf manual.pdf *.ps *.png $(HOME)/web/bjeannet-forge/interproc
	cp manual*.html $(HOME)/web/bjeannet-forge/interproc

online: interprocweb.cgi interprocwebf.cgi 
	chmod a+rX $^
	cp $^ /home/wwwpop-art/pub/interproc
	cp -r examples/*.txt /home/wwwpop-art/pub/interproc/interproc_examples

#--------------------------------------------------------------
# IMPLICIT RULES AND DEPENDENCIES
#--------------------------------------------------------------

.SUFFIXES: .ml .mli .cmi .cmo .cmx

#-----------------------------------
# CAML
#-----------------------------------

%.ml: %.mll
	$(OCAMLLEX) $^

%.ml %.mli: %.mly
	$(OCAMLYACC) $^

%.cmi: %.mli
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) $(OCAMLINC) -package $(REQ_PKG) -c $<

%.cmo: %.ml
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) $(OCAMLINC) -package $(REQ_PKG) -c $<

%.cmx: %.ml
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLINC) -package $(REQ_PKG) -c $<

depend: spl_yacc.ml spl_yacc.mli spl_lex.ml
	$(OCAMLFIND) ocamldep $(MLMODULES:%=%.ml) $(MLMODULES:%=%.mli) interproc.ml interproc.mli interprocweb.ml interprocweb.mli mainweb.ml mainwebf.ml >Makefile.depend

Makefile.depend: spl_yacc.ml spl_yacc.mli spl_lex.ml
	$(OCAMLFIND) ocamldep $(MLMODULES:%=%.ml) $(MLMODULES:%=%.mli) interproc.ml interproc.mli interprocweb.ml interprocweb.mli mainweb.ml mainwebf.ml >Makefile.depend

-include Makefile.depend
