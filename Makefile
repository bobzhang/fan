VERSION = 0.1
export VERSION

PREFIX ?= $(shell dirname $(shell dirname `ocamlc -where`))
BINDIR ?= $(PREFIX)/bin
OCAMLBUILD ?= ocamlbuild


COLD=cold
SRC=src

BCOLD=_build/cold
BHOT=_build/src
EXES=fan.byte fan.native


build:
	$(OCAMLBUILD) cold/fan.byte cold/fan.native \
	cold/gram.cma cold/rts.cma


install:
	make libinstall
	make bininstall


bininstall:
	install -m 0755 _build/cold/fan.byte _build/cold/fan.native $(BINDIR)


hotworld:
	make hotbuild
	make uninstall
	make hotinstall
hotbuild:
	$(OCAMLBUILD) src/fan.byte src/fan.native \
	src/gram.cma src/rts.cma

hotinstall:
	install -m 0755 _build/src/fan.byte _build/src/fan.native $(BINDIR)
	ocamlfind install fan META $(BHOT)/*.cmi $(BHOT)/rts.cma $(BHOT)/gram.cma


TARGETS = gram.cma gram.cmx gram.cmxs rts.cma rts.cmx rts.cmxs

libinstall:
	ocamlfind install fan META $(BCOLD)/*.cmi $(BCOLD)/rts.cma $(BCOLD)/gram.cma

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

.PHONY: top

