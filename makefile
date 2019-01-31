OCAMLC=ocamlc
OCAMLOPT=ocamlfind ocamlopt

EXE=cohd

INCLUDES=-I "/Users/ltheveno/soft/Frontc-3.4/frontc"
PACKAGES=-package str -linkpkg 

OCAMLFLAGS= $(INCLUDES) $(LIBS)

SRCS =  types.cmx libcommon.cmx config.cmx commandline.cmx irtodot.cmx irtoc.cmx irtree.cmx astcommon.cmx passcommon.cmx preprocess.cmx highop.cmx highprec.cmx tradeoff.cmx tac.cmx passes.cmx irbuilder.cmx cohd.cmx

COHD = $(SRCS)
COHDO = unix.cmxa frontc.cmxa $(SRCS)

cohd: $(COHD)
	$(OCAMLOPT) $(PACKAGES) $(OCAMLFLAGS) -o $(EXE) $(COHDO)

# Common rules
.SUFFIXES: .ml .mli .cmx

.mli.ml:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(PACKAGES) $(OCAMLFLAGS) -c $<

# Clean up
clean:
	rm -f $(EXE)
	rm -f *.cm[iox]
	rm -f *.o
	rm -f *~
	rm -f *~

doc: cohd
	ocamldoc -html -colorize-code -hide Cabs,Frontc -d ./doc/ -t COHD *.ml
