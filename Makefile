
.PHONY:un-install

un-install:
	ocamlfind remove fan
install:
	ocamlfind install fan META src/utils/libutils.cma src/common/libcommon.cma src/treeparser/libtreeparser.cma  src/cold/libmain_top.cma src/utils/*.cmi src/common/*.cmi src/treeparser/*.cmi src/cold/*.cmi
