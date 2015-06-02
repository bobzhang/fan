+OPTIONS: toc:nil html-postamble:nil html-preamble:nil
======================================================

+HTML\_HEAD: 
=============

+TITLE: Fan's syntax
====================

+OPTIONS: ^:
============

+OPTIONS: toc:nil
=================

Fan speaks OCaml natively, plus a few addons.

There are some minor differences between Fan's concrete syntax and OCaml
though, the major differences is that Fan is more strict than OCaml.

-  Difference from Caml syntax Three particular points to be noticed:

1. Parens are necessary for tuples #+BEGIN\_SRC ocaml (\*\* illegal \*)
   a,b let a,b = f in body #+END\_SRC

   #+BEGIN\_SRC ocaml (\*\* correct syntax \*) (a,b) let (a,b) = f in
   body #+END\_SRC
2. Parens or "begin" "end" necessary for semis #+BEGIN\_SRC ocaml (\*
   illegal \*) print\_endline "a"; print\_endline "b" #+END\_SRC
   #+BEGIN\_SRC ocaml (\*\* correct \*) (print\_endline "a";
   print\_endline "b") begin print\_endline "a"; print\_endline "b" end
   #+END\_SRC
3. First vertical bar is necessary for algebraic data type, pattern
   match. #+BEGIN\_SRC ocaml (\*\* illegal \*) type u = A \| B let f =
   function A -> "a" \| B -> "b" let f = match c with A -> "a" \| B ->
   "b" #+END\_SRC

   #+BEGIN\_SRC ocaml (\*\* correct \*) type u = \| A \| B

   let f = function \| A -> "a" \| B -> "b"

   let f = match c with \| A -> "a" \| B -> "b"

   #+END\_SRC
4. $ is a reserved operator, please don't take it as a function.

-  Introduced syntax

It mainly introduce two syntaxes, we call them delimited domain specific
languages(DDSL).

1. compile time quotation

   | #+BEGIN\_SRC ocaml #{:quot\| \|}
   |  #+END\_SRC

   Such quotation is purely compile time control language, it's a
   general view of such directives:

   #+BEGIN\_SRC ocaml #load "xx.cmo";; #use "xx.ml" ;; #+END\_SRC

   It can be used to interact with the Fan compiler, the user can
   register different compile time quotation language to make the whole
   file self-explainable, this helps simplify the build system a lot.

   For example, there is a built-in control language

   | #+BEGIN\_SRC ocaml #{:control\| default "exp"; (\* set the default
   quotation *) require "ulex"; (* require the feature\ *) import
   "Tutorial"; (* import the language namespace\ *) filter "serialize";
   (* apply the filter serialize to the whole file \*) \|}
   |  #+END\_SRC

   Another language eval, could eval all legal ocaml code at
   compile-time.

   | #+BEGIN\_SRC ocaml #{:eval\| let v = 3 let b = 3 \|}
   |  #+END\_SRC

   This would help test the syntax extension on-the-fly.

   *Note*: all the compile time DDSL should be put in the beginning,
   this is intentional, to work better with IDE.

2. Rewrite DDSL.

   The syntax is the same with compile-time DDSL except without hash
   sign, one sample is as follows:

   #+BEGIN\_SRC ocaml {:exp\| 3 \|} #+END\_SRC

   It can appear in most places (detail later)


