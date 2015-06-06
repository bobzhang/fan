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

It installed three exectuables: ``fan.byte``,
``fan.native``, ``fane.byte`` into the path: ``ocamlc -where/bin``.
All the library files are installed into :sub:```ocamlc -where/fan``.

``FAst.cmi`` and ``FAstN.cmi`` two signature files are installed to
the path of standard library.

Please read ``META`` for more details.

``fan.byte``, ``fan.native`` have exactly the same functionality, they
are stand-alone without linking any external libraries (no
compiler-libs linked, no dynlink).

fane.byte links the whole byte-code compiler and toplevel, it provides
*eval* functionality, which we will talk about later.



What language does Fan speak?
-----------------------------

Fan speaks OCaml natively, with a few addons.

There are some minor differences between Fan's concrete syntax and OCaml
though, the major differences is that Fan is more strict than OCaml.

..
   -   Three particular points to be noticed:

.. note:: difference from OCaml syntax

   #. Parens are necessary for tuples

      .. code-block:: ocaml
      
         (* incorrect *)
         a,b  
         (* correct *)
         (a,b) 

         (*incorrect *)
         let a,b = f in body 
         (* correct *)
         let (a,b) = f in body 

   #. Parens or *begin* *end* necessary for semis
       
      .. code-block:: ocaml

          (* incorrect *)
          print_endline "a"; 
          print_endline "b"

          (* correct *)
          (print_endline "a";
          print_endline "b")
          (* correct *)
          begin
           print_endline "a";
           print_endline "b" 
          end

   #. First vertical bar is necessary for algebraic data type, pattern
      match.
      
      .. code-block:: ocaml

          (* incorrect *) 
          type u = A | B
          let f = function
               A -> "a" 
             | B -> "b"
          let f = 
            match c with
              A -> "a" 
            | B -> "b"
          
          (* correct *)  
          type u = | A | B

          let f = function | A -> "a" | B -> "b"
          let f = match c with | A -> "a" | B -> "b"

   #.  ``$`` is a reserved operator, please don't take it as a function.

..  note:: added syntax

    It mainly introduce two syntaxes, we call them delimited domain specific
    languages(DDSL).

    #. compile time quotation
       
       .. code-block:: ocaml
          
          #{:quot| |}

       Such quotation is purely compile time control language, it's a
       general view of such directives:

       .. code-block:: ocaml
                       
          #load "xx.cmo";;
          #use "xx.ml" ;; 

       It can be used to interact with the Fan compiler, the user can
       register different compile time quotation language to make the whole
       file self-explainable, this helps simplify the build system a lot.

       For example, there is a built-in control language

       .. code-block:: ocaml

          #{:control| 
          default "exp"; (* set the default quotation *) 
          require "ulex"; (* require the feature *)
          import "Tutorial"; (* import the language namespace *) 
          filter "serialize"; (* apply the filter serialize to the whole file *) 
          |}


       Another language ``eval``, could eval all legal ocaml code at
       compile-time.
       
       .. code-block:: ocaml

          #eval{
          let v = 3 let b = 3 
          }
       
       This would help test the syntax extension on-the-fly.

       **Attention**: all the compile time DDSL should be put in the beginning,
       this is intentional, to work better with IDE.

    #. Rewrite DDSL.

       The syntax is the same with compile-time DDSL except without hash
       sign, one sample is as follows:

       .. code-block:: ocaml
          
          %exp{ 3}


       It can appear in most places (detail later)






-  Fan for toplevel See [[file:toplevel\_support.org][Toplevel support]]

-  Fan's quotation system see [[file:quotation\_system.org][Fan's
   quasiquotation]]

-  Fan's parsing scheme see [[file:parser.org][parser DDSL]]
-  Fan's lexing scheme see [[file:lexer.org][lexer DDSL]]

-  Writing plugins for Fan see [[file:plugins.org][Plugins]]


.. include:: ./link_names.txt
