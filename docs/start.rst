===============
Getting started
===============

The following post assumes the reader is already familiar with
OCaml. If you are not familiar with OCaml, we sugget you to get
started here: `OCaml tutorial`_. Learning OCaml would help you become
a better programmer :-)


Installation
============


Requirements

   #. `OCaml (version >=4) <http://caml.inria.fr/ocaml/release.en.html>`_
      
   #. `OCamlfind`_

.. highlight:: sh

::

   make world

If you have ocamlfind installed, it will install a META file will be installed to the corresponding
place, otherwise no META file installed

What it installed?
------------------

It installed three exectuables: *fan.byte*,
*fan.native*, *fane.byte* into the path: :sub:`ocamlc -where`/bin`.

All the library files are installed into :sub:```ocamlc -where``/fan`.

FAst.cmi and FAstN.cmi two signature files are installed to the path of
standard library.

Please read META file for more details.

fan.byte, fan.native have exactly the same functionality, they are stand
alone without linking any external libraries (no compiler-libs linked,
no dynlink).

fane.byte links the whole byte-code compiler and toplevel, it provides
*eval* functionality, which we will talk about later.



-  What language does Fan speak? See [[file:syntax.org][syntax]]

-  A high-level overview of Fan's architecture. See
   [[file:overview.org][overview]].

-  Compiling with Fan See [[file:compilation.org][compilation]]

-  Fan for toplevel See [[file:toplevel\_support.org][Toplevel support]]

-  Fan's quotation system see [[file:quotation\_system.org][Fan's
   quasiquotation]]

-  Fan's parsing scheme see [[file:parser.org][parser DDSL]]
-  Fan's lexing scheme see [[file:lexer.org][lexer DDSL]]

-  Writing plugins for Fan see [[file:plugins.org][Plugins]]


.. include:: ./link_names.txt
