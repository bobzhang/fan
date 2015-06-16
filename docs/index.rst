Fan: metaprogramming for OCaml
======================================


.. toctree::
   :hidden: 
              
   start
   philosophy
   compilation
   ddsl
   lexer
   toplevel
   plugins
   parser
   quotation_system
   browser
   papers
   dev
   credits

.. note::

   Documentation of Fan is a bit out-of-date, it will be updated
   recently.





Fan_ is a compile-time metaprogramming system for OCaml_,
originally inspired from Camlp4_. It's a combination of OCaml and
`Lispy Macros`_. It shares the same concrete syntax with OCaml.

There is an online REPL that user can play with iFan_


Status
------

Fan is still in active development, so it's expected there would be
many bugs. This also means that your participation is particularly
important, and your suggestions will affect the design of Fan.

Contributions
-------------

Contributions are very welcome

#. `Link to Fan <Fan>`_

#. `Link to Fan's Documentation <Fan Documentation>`_

It's written in `ReST`_ markup language and powered by Sphinx_. 


..
   -  COMMENT TODO List

   -  Porting Documentation on Camlp4 to Fan

   -  Native conditional compilation

   -  Direct support for type-conv, or meta-conv It's already in the repo,
      undocumented though.

   -  Add eval support for byte code

   -  Adding a directive language at the beginning of files

   -  Backport Fan to OCaml 3.12

   -  Scheme-style macros for local ast rewriting without computation

   -  COMMENT It shares the same run-time with OCaml,

..
   By all means, master the spirit of metaprogramming would

   for example, function is the most basic concept in nearly all
   programming languages,

..
   Metaprogramming enables the possiblity for the user to write least lines
   of code if they like. As a programmer, [[http://threevirtues.com/][being
   lazy]] should always be appreciated, we will show you some examples that
   metaprogramming can turn your tens lines of code into thousands of lines
   of code, simply put, metaprogramming is so invaluable that every
   practical programmer should master such technology.

   That said, we need a macro system for your favorite programming
   language. [[http://www.sbcl.org/][Common Lisp]] has been the best
   language for metaprogramming during the last decades. However, as one of
   the eldest languages, there are some serious design defects for this
   language: first, it does not support separate compilation, there is no
   clear bound between compile-time runtime and

   Hmm, a good question, it's simply because writing boilerplate code is
   tedious and error prone, and

   Having programming in
   [[http://en.wikipedia.org/wiki/Common\_Lisp][Common Lisp]] for years, I
   happened to find
   `F# <http://research.microsoft.com/en-us/projects/fsharp/>`__ when I was
   doing an internship at Microsoft. I was fascinated by the beauty of
   functional languages, for example, the algebraic data type combined with
   pattern match is superior to s-expression in some regards, and strongly
   typed languages with type inference does help, especially when you write
   large software and do the refactorization, a type system is the best
   tool for refactorization. XD.


.. include:: ./link_names.txt
