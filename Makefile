VERSION = 0.1
export VERSION

PREFIX ?= $(shell dirname $(shell dirname `ocamlc -where`))
LIBPREFIX ?= `ocamlc -where`
# the path to install the basic cmi files
LIBDIR ?= $(LIBPREFIX)/fan



# the path to install binary
BINDIR ?= $(PREFIX)/bin

OCAMLBUILD ?= ocamlbuild





BCOLD=_build/cold/
BHOT=_build/src/
EXES=fan.byte fan.native fane.byte

LIBTARGETS = fgram.cma fgram.cmx fgram.cmxs rts.cma rts.cmxa rts.cmxs fanTop.cma mktop.cma mkFan.cma
BINTARGETS = fan.byte fan.native fane.byte


STDTARGETS = fAst.cmi fAstN.cmi

ICTARGETS=$(addprefix _build/cold,$(TARGETS))

BYTEXLIBS = mkFan.cma fEval.cmo 




build:
	$(OCAMLBUILD) $(addprefix cold/,$(LIBTARGETS) $(BINTARGETS))



install:
	install -m 0755 $(addprefix $(BCOLD), $(BINTARGETS)) $(BINDIR)
	@[ -f `which ocamlfind` ] && make metainstall
	if ! [ -a $(LIBDIR) ]; then mkdir $(LIBDIR); fi;
	echo "installing to " $(LIBDIR)
	install -m 0755 $(addprefix $(BCOLD), FAstN.cmi FAst.cmi) $(LIBPREFIX)
	install -m 0755 $(BCOLD)*.cmi $(addprefix $(BCOLD), $(LIBTARGETS)) $(LIBDIR)

metainstall:
	@[ -d `ocamlfind query fan` ] && ocamlfind remove fan
	ocamlfind install fan META
world:
	make build
	# make uninstall
	make install

hotworld:
	make hotbuild
	# make uninstall
	make hotinstall

hotinstall:
	install -m 0755 $(addprefix $(BHOT), $(BINTARGETS)) $(BINDIR)
	@[ -f `which ocamlfind` ] && make metainstall
	if ! [ -a $(LIBDIR) ]; then mkdir $(LIBDIR); fi;
	echo "installing to " $(LIBDIR)
	install -m 0755 $(addprefix $(BHOT), FAstN.cmi FAst.cmi) $(LIBPREFIX)
	install -m 0755 $(BHOT)*.cmi $(addprefix $(BHOT), $(LIBTARGETS)) $(LIBDIR)

hotbuild:
	$(OCAMLBUILD) $(addprefix src/,$(LIBTARGETS) $(BINTARGETS))


uninstall:
	make libuninstall

libuninstall:
	ocamlfind remove fan 

top:
	ocamlbuild -I src foo.otarget

cleansrc:
	rm -rf _build/src
cleancold:
	rm -rf _build/cold
cleantmp:
	rm -rf _build/tmp
cleandemo:
	rm -rf _build/demo
cleantest:
	rm -rf _build/test

boot:
	cd ~/fan/ && ocamlbuild -I src boot/fan.native
stat:
	rm -rf stat/*
	git_stats . stat


# ls *.mli | sed s/.mli$// > foo.odocl
doc:
	ocamlbuild src/foo.docdir/index.html
updoc:
	make doc
	rm -rf ~/Dropbox/fanweb/foo.docdir
	mv _build/src/foo.docdir ~/Dropbox/fanweb/

.PHONY: top doc byteX
