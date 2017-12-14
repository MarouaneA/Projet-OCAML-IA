COMPILE = ocamlc.opt
COMPILEOPT = ocamlopt.opt
CSLDEP=ocamldep
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
CAMLDOC= ocamldoc

NORM_FILES= generation.ml interface.ml main.ml
NORM_OBJS =  $(NORM_FILES:.ml=.cmo)
OPT_OBJS=  $(NORM_FILES:.ml=.cmx)

INCLUDES = -I +labltk
LIBS = labltk.cma
LIBSOPT = $(LIBS:.cma=.cmxa)

all : labyrinthe

labyrinthe : $(OPT_OBJS)
	$(COMPILEOPT) -o $@ $(INCLUDES) $(LIBSOPT) $(OPT_OBJS)

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.mll.mli:
	$(CAMLLEX) $<
.mll.ml:
	$(CAMLLEX) $<
.mly.mli:
	$(CAMLYACC) $<
.mly.ml:
	$(CAMLYACC) $<
.ml.cmo :
	$(COMPILE) -c $(INCLUDES) $<
.mli.cmi :
	$(COMPILE) -c $(INCLUDES) $<
.ml.cmx :
	$(COMPILEOPT) -c $(INCLUDES) $<

cleandoc:
	\rm -Rf doc

clean:
	\rm -f *.cmo *.cmi *.cmx *.o *.a *.cma *.cmxa .depend *~ labyrinthe

.depend:
	$(CSLDEP) *.mli *.ml >.depend

include .depend
