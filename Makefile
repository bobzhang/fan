
SPHINXBUILD   = sphinx-build
BUILDDIR      = build


ALLSPHINXOPTS   = -d $(BUILDDIR)/doctrees  $(SPHINXOPTS) docs

.PHONY: help clean html un-install


clean:
	rm -rf $(BUILDDIR)/*
pub:
	ghp-import  -p build/html
html:
	sphinx-build -j8 -b html -d build/doctrees docs build/html
	touch build/html/.nojekyll
	@echo
	@echo "Build finished. The HTML pages are in $(BUILDDIR)/html."



all:
	cd src ; pmake fan_top
un-install:
	ocamlfind remove fan
install:
	ocamlfind install fan META src/utils/libutils.cma src/common/libcommon.cma src/treeparser/libtreeparser.cma  src/cold/libmain_top.cma src/utils/*.cmi src/common/*.cmi src/treeparser/*.cmi src/cold/*.cmi

