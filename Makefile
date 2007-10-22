include Makefile.config

#---------------------------------------
# variables utiles
#---------------------------------------

# For the WEB version
OCAMLHTML_INSTALL = $(HOME)/pkg/ocamlhtml/$(ARCH)

#
LCFLAGS = \
-L$(GMP_PREFIX)/lib \
-L$(MPFR_PREFIX)/lib \
-L$(APRON_PREFIX)/lib \
-L$(CAML_PREFIX)/lib/ocaml \
-L$(CAMLIDL_PREFIX)/lib/ocaml \
-L$(ANALYZER_PREFIX)/lib


OCAMLLDFLAGS = \
-g -noautolink unix.cma bigarray.cma camllib.cma analyzer.cma gmp.cma apron.cma box.cma oct.cma polka.cma ppl.cma polkaGrid.cma -cc "g++" -ccopt "-L$(CAML_PREFIX)/lib/ocaml -L$(CAMLIDL_PREFIX)/lib/ocaml -L$(APRON_PREFIX)/lib -L$(GMP_PREFIX)/lib -L$(MPFR_PREFIX)/lib -L$(PPL_PREFIX)/lib" -cclib "-lpolkaGrid_caml_debug -lap_pkgrid_debug -lap_ppl_caml -lap_ppl_debug -lppl -lgmpxx -lpolka_caml_debug -lpolka_debug -loct_caml -loct -lbox_caml_debug -lbox_debug -lapron_caml_debug -lapron_debug -lgmp_caml -lmpfr -lgmp -lunix -lbigarray -lcamlidl"

OCAMLOPTLDFLAGS = \
-g -noautolink unix.cmxa bigarray.cmxa camllib.cmxa analyzer.cmxa gmp.cmxa apron.cmxa box.cmxa oct.cmxa polka.cmxa ppl.cmxa polkaGrid.cmxa \
-cc "g++" -ccopt "-L$(CAML_PREFIX)/lib/ocaml -L$(CAMLIDL_PREFIX)/lib/ocaml -L$(APRON_PREFIX)/lib -L$(GMP_PREFIX)/lib -L$(MPFR_PREFIX)/lib -L$(PPL_PREFIX)/lib" -cclib "-lpolkaGrid_caml_debug -lap_pkgrid_debug -lap_ppl_caml -lap_ppl_debug -lppl -lgmpxx -lpolka_caml_debug -lpolka_debug -loct_caml -loct -lbox_caml_debug -lbox_debug -lapron_caml_debug -lapron_debug -lgmp_caml -lmpfr -lgmp -lunix -lbigarray -lcamlidl"

OCAMLINC = \
-I $(OCAMLHTML_INSTALL)/lib \
-I $(CAMLLIB_PREFIX)/lib \
-I $(APRON_PREFIX)/lib \
-I $(CAML_PREFIX)/lib/ocaml \
-I $(CAMLIDL_PREFIX)/lib/ocaml


MLMODULES = spl_syn pSpl_syn spl_yacc spl_lex boolexpr equation syn2equation solving option frontend
MLSRC =  $(MLMODULES:%=%.ml) $(MLMODULES:%=%.mli)

INT = $(MLMODULES:%=%.cmi)
OBJ = $(MLMODULES:%=%.cmo)
OBJx = $(MLMODULES:%=%.cmx)


#---------------------------------------
# Rules
#---------------------------------------

# Global rules
all: interproc
opt: interproc.opt

interproc: $(OBJ) interproc.cmo apronrun
	$(OCAMLC) -g -o $@ -use-runtime ./apronrun $(OCAMLFLAGS) $(OCAMLINC) \
	bigarray.cma unix.cma camllib.cma analyzer.cma gmp.cma		\
	apron.cma polka.cma box.cma ppl.cma polkaGrid.cma analyzer.cma $(OBJ) interproc.cmo

apronrun:
	$(OCAMLC) -g -o $@ -make-runtime $(OCAMLINC) $(OCAMLLDFLAGS)

interproc.opt: $(OBJx) interproc.cmx
	$(OCAMLOPT) -o $@ -verbose $(OCAMLINC) $(OCAMLOPTLDFLAGS) \
	analyzer.cmxa $(OBJx) interproc.cmx

interprocweb.cgi: $(OBJx) interprocweb.cmx 
	$(OCAMLOPT) -o $@ -verbose $(OCAMLINC) $(OCAMLOPTLDFLAGS) \
	analyzer.cmxa html.cmxa $(OBJx) interprocweb.cmx 

interprocweb: $(OBJ) interprocweb.cmo 
	$(OCAMLC) -g -o $@ -use-runtime ./apronrun -verbose $(OCAMLINC) $(OCAMLLDFLAGS) \
	analyzer.cma html.cma $(OBJ) interprocweb.cmo 

mostlyclean: clean
	$(RM) -r *.pdf html

clean:
	$(RM) *.[aoc] *.cm[ioxa] *.annot spl_lex.ml spl_yacc.ml spl_yacc.mli interproc apronrun interproc.opt interprocweb interprocweb.cgi *~ *.idx *.ilg *.ind *.log *.toc *.dvi *.out *.aux *.bbl *.blg *.makeimage *.html *.png *.ps ocamldoc.* *.output

#---------------------------------------
# TEX and HTML rules
#---------------------------------------

.PHONY: html interproc.dvi manual.dvi

manual.pdf: manual.dvi
	$(DVIPDF) manual.dvi

manual.dvi:
	latex manual
	bibtex manual
	latex manual
	latex manual

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
	hyperlatex manual	
	cp -r html interproc.pdf manual.pdf $(HOME)/web/bjeannet-forge/interproc
	cp manual*.html $(HOME)/web/bjeannet-forge/interproc

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
	$(OCAMLC) -g $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmo: %.ml
	$(OCAMLC) -g $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $<

Makefile.depend: spl_yacc.ml spl_yacc.mli spl_lex.ml
	$(OCAMLDEP) $(MLSRC) interproc.ml interproc.mli interprocweb.ml interprocweb.mli >Makefile.depend

-include Makefile.depend
