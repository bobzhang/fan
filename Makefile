VERSION = 0.1
export VERSION

PREFIX ?= $(shell dirname $(shell dirname `ocamlc -where`))

# the path to install the basic cmi files
LIBDIR ?= `ocamlc -where`

# the path to install binary
BINDIR ?= $(PREFIX)/bin

OCAMLBUILD ?= ocamlbuild


COLD=cold
SRC=src

BCOLD=_build/cold
BHOT=_build/src
EXES=fan.byte fan.native

LIBTARGETS = gram.cma gram.cmx gram.cmxs rts.cma rts.cmxa rts.cmxs fanTop.cma
BINTARGETS = fan.byte fan.native 


STDTARGETS = fAst.cmi fAstN.cmi

ICTARGETS=$(addprefix _build/cold,$(TARGETS))

build:
	$(OCAMLBUILD) $(addprefix cold/,$(LIBTARGETS) $(BINTARGETS))


install:
	ocamlfind install fan META $(BCOLD)/*.cmi $(addprefix _build/cold/, $(LIBTARGETS))
	install -m 0755 $(addprefix _build/cold/, $(BINTARGETS)) $(BINDIR)
	install -m 0755 $(addprefix _build/cold/, $(STDTARGETS)) $(LIBDIR)
world:
	make build
	make uninstall
	make install

hotinstall:
	ocamlfind install fan META $(BHOT)/*.cmi $(addprefix _build/src, $(LIBTARGETS))
	install -m 0755 $(addprefix _build/src/, $(BINTARGETS)) $(BINDIR)
	install -m 0755 $(addprefix _build/src/, $(STDTARGETS)) $(LIBDIR)

hotworld:
	make hotbuild
	make uninstall
	make hotinstall
hotbuild:
	$(OCAMLBUILD) 	$(OCAMLBUILD) $(addprefix src/,$(LIBTARGETS) $(BINTARGETS))


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
	cd ~/fan/ && ocamlbuild -I src boot/FanDriver.native
stat:
	rm -rf stat/*
	git_stats . stat



doc:
	ocamlbuild src/foo.docdir/index.html
updoc:
	make doc
	rm -rf ~/Dropbox/fanweb/Fan/foo.docdir
	mv _build/src/foo.docdir ~/Dropbox/fanweb/Fan/

.PHONY: top doc