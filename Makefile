VERSION = 0.1
export VERSION
ifndef PREFIX
  PREFIX = $(shell dirname $(shell dirname `which ocamlc`))
endif

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
endif

ifndef OB
  OB = ocamlbuild
endif

build:
	
install:
	install -m 0755 fan $
NAME = fan
REQUIRES=

TARGETS = gram.cma gram.cmx gram.cmxs rts.cma rts.cmx rts.cmxs

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

install:
	ocamlfind install *.cmi
