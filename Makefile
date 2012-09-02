
OB = ocamlbuild



cleanlib:
	rm -rf _build/src/*.cm[oaix]
	rm -rf _build/src/*.o
	rm -rf _build/src/*.depends
	rm -rf _build/src/Camlp4
	rm -rf _build/src/*.ml.depends
