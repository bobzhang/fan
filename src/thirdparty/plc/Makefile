
# make plc.cma to build Prolog plugin
# make demo1 to build (very) simple demo

# need treeparser library for Gstructure (refed by gramf).  Otherwise get odd errrors.
INCLUDES=-I ../../utils -I ../../common -I ../../treeparser -I ../../cold
CMOS=PlEnv.cmo PlVersion.cmo PlAst.cmo PlNames.cmo PlTranslate.cmo PlParser.cmo

# Uncomment SHOW to output the result of Fan translation
# SHOW=-dsource

%.cmo: %.ml
	ocamlc -pp '../../fan' $(INCLUDES) -c $<

%.cmi: %.mli
	ocamlc -c $<

plc.cma: PlEnv.cmi $(CMOS)
	ocamlc $(INCLUDES) -o $@ -a -linkall $(CMOS)

# Use fan.run 'coz we only build a cma, not a cmxs
# (build fan.run with ./pmake fan.run)
# (argument -plugin wants only the base filename)
demo1: demo.ml plc.cma
	ocamlc -pp '../../fan.run -plugin plc' $(SHOW) -o demo1 demo1.ml

clean:
	rm -f *.annot
	rm -f *.cm*
	rm -f demo1