+OPTIONS: toc:nil html-postamble:nil html-preamble:nil
======================================================

+HTML\_HEAD: 
=============

+TITLE: Command Line compilation Using Fan
==========================================

+OPTIONS: ^:
============

+OPTIONS: toc:nil
=================

+TOC:headines 2
===============

-  Hello world <> Create a file [[file:code/hello.ml][hello.ml]] as
   follows:

   +INCLUDE:"./code/hello.ml" src ocaml :lines "1-3"
   =================================================

   The compile is quite simple, make sure =fan.byte= or =fan.native= is
   in your search path.

   +BEGIN\_SRC sh
   ==============

   $ ocamlc -pp 'fan.native' hello.ml -o test $ ./test #+END\_SRC

   As you may notice, adding ~-pp 'fan.native'~ flag is enough to
   switching to Fan. Using =fan.byte= or =fan.native= is up to you, for
   the time being, only the performance matters here. So, compiling with
   the following command line does also work.

   +BEGIN\_SRC sh
   ==============

   | $ ocamlc -pp 'fan.native' hello.ml -o test
   | #+END\_SRC

-  First class lexer

Writing hello world is not very interesting, for the following example,
we show you how DDSL fits into Fan. Suppose we want to write a lexical
analyzier to filter nested comments in OCaml, the traditional way is to
write a complex regex expression, or start a new file to write a lexer.
The first way is hackish, inefficient, unmaintainable in the long run
while the second way is too heavy weight, since lexer generator is a
standard alone external DDSL which introduces another staging phase.

Within Fan, we show how easy it is now: #+INCLUDE: "./code/comment.ml"
src ocaml :lines "1-27"

Compiling is the same as the previous example [[hello]]

| #+BEGIN\_SRC sh ocamlc -annot -pp 'fan.native' comment.ml -o comment
|  #+END\_SRC

Here we see the lexer DDSL is first class construct in Fan, the user
don't need to create a new file to isolate their lexer, it's as
convenient as regex expression in perl. So it works in toplevel, it
works with module system, and objects, that said, the user could make
lexer reusable by using
[[http://caml.inria.fr/pub/docs/manual-ocaml/manual005.html][objects]]
instead of functions.

Abot the internal of /lexer/ DDSL, see
[[file:ddsl/lexer.org][DDSL:lexer]].