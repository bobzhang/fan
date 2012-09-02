
OB = ocamlbuild



cleanlib:
	rm -rf _build/*.cm[oaix]
	rm -rf _build/*.o
	rm -rf _build/*.depends
	rm -rf _build/Camlp4
	rm -rf _build/*.ml.depends
