# -*- Mode:Shell-script -*-
ocamlc -I +camlp4 dynlink.cma camlp4lib.cma Camlp4Bin.cmo -linkall -o camlp6

ocamlc -I +camlp4 dynlink.cma unix.cma camlp4lib.cma Camlp4Bin.cmo -I +camlp4/Camlp4Parsers Camlp4OCamlRevisedParser.cmo Camlp4QuotationCommon.cmo Camlp4QuotationExpander.cmo Camlp4OCamlRevisedParserParser.cmo Camlp4GrammarParser.cmo Camlp4MacroParser.cmo Camlp4ListComprehension.cmo -I +camlp4/Camlp4Printers Camlp4AutoPrinter.cmo fan.cma -linkall -o camlp6
