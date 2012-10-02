
OB = ocamlbuild
OF = ocamlfind 

all:camlp4t
	$(OB) fan.otarget

camlp4t:
	$(OB) eval.cma
	$(OB) camlp4trf.byte
	$(OB) camlp4to.byte

rebuild: clean remove all install

install:
	$(OF) install camlp4util META _build/*.cm[aoxi]
remove:
	$(OF) remove camlp4util
clean:
	# $(OB) -clean
	rm -rf _build/*.ml
	rm -rf _build/*.cm[oaxi]
cleandoc:
	rm -rf _build/foo.docdir
doc:
	$(OB) foo.otarget
	chmod -R 755 _build
	rm -rf ../fan_doc
	cp ./_build/foo.docdir ../fan_doc
	chmod -R 755 ../fan_doc/
installdoc:
	rm -rf ../fan_doc/
	cp -r _build/foo.docdir ../fan_doc
tags:
	otags -pc -pa rf  -r -q . 
test: clean
	# $(OB) camlp4util.cma
	# rm -f _build/typedef.cmo _build/testlift.cmo _buildtypedef.cma
	$(OB) test.cma

.PHONY: clean all test doc plugins tags

%.mli : %.ml
	rm -f $@
	$(OB) $(basename $@).inferred.mli
	cp _build/$(basename $@).inferred.mli $@

COMPILE_PATH=`ocamlfind query compiler-libs`
STDLIB_PATH=`ocamlfind query stdlib`
INCLUDES=-I $(COMPILE_PATH) -I $(STDLIB_PATH)
DUMP=./cmidump.byte
TARGETS=lexing ident longident
cmp_interf:
	for target in $(TARGETS); do \
		$(DUMP) $(INCLUDES) $$target.cmi > $$target.i.mli;\
	done
