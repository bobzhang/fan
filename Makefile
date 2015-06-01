

SPHINXBUILD   = sphinx-build
BUILDDIR      = build


ALLSPHINXOPTS   = -d $(BUILDDIR)/doctrees  $(SPHINXOPTS) docs

.PHONY: help clean html


clean:
	rm -rf $(BUILDDIR)/*
pub:
	ghp-import  build/html
html:
	sphinx-build -b html -d build/doctrees docs build/html
	touch build/html/.nojekyll
	@echo
	@echo "Build finished. The HTML pages are in $(BUILDDIR)/html."

