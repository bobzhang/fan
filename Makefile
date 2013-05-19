
OB = ocamlbuild


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