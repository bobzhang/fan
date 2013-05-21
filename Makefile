VERSION = 0.1
export VERSION

PREFIX ?= $(shell dirname $(shell dirname `which ocamlc`))
BINDIR ?= $(PREFIX)/bin
OCAMLBUILD ?= ocamlbuild


COLD=cold
SRC=src
BCOLD=_build/cold

EXES=fan.byte fan.native


build:
	$(OCAMLBUILD) cold/fan.byte cold/fan.native \
	cold/gram.cma cold/rts.cma

install:
	make libinstall
	make bininstall
bininstall:
	install -m 0755 _build/cold/fan.byte _build/cold/fan.native $(BINDIR)

TARGETS = gram.cma gram.cmx gram.cmxs rts.cma rts.cmx rts.cmxs

libinstall:
	ocamlfind install fan META $(BCOLD)/*.cmi $(BCOLD)/rts.cma $(BCOLD)/gram.cma

libuninstall:
	ocamlfind uninstall fan 

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

