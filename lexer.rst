+OPTIONS: toc:nil html-postamble:nil html-preamble:nil
======================================================

+HTML\_HEAD: 
=============

+TITLE: Lexer DDSL
==================

+OPTIONS: ^:
============

+OPTIONS: toc:nil
=================

+TOC:headines 2
===============

/lexer/ DDSL is just another backend for ocamllex, the nice
[[http://pllab.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamllex-tutorial/index.html][tutorial]]
about ocamllex would also help understand lexer DDSL.

-  Usage

| \*\* Syntax lexer DDSL's syntax is similar to ocamllex. #+BEGIN\_SRC
ocaml {:regexp\| let name1 = r1 let name2 = r2 ...
|  let name n = rn
|  \|};; (\*\* shortest = false\*)
|  {:lexer\| \| r1 -> e1 \| r2 -> e2 \| r3 -> e3 \|};;

::

     (** shortest = true *)
     {:lexer|
      < r1 -> e1
      | r2 ->e2
      | r3 -> e3
      |}  

#+END\_SRC

Here r\_{n} stands for regex expression, e\_{n} means Fan's expression,
different lexer DDSLs are guaranteed to be independent.

Similar to ocamllex, /lexbuf/ is available to actions.

Keep in mind, /lexer/ DDSL is simply an expression, it works with any
feature provided by OCaml. That said, in the action, you can start
another lexer as you want or you may put the lexer DDSL as a body of a
method definition.

The syntax for regex is the same as ocamllex, it supports all features
provided by ocamllex except two aspects:

1. "!" stands end of file instead of "eof" in ocamllex
2. lexer DDSL's lexing convention is more restrict.

   #+BEGIN\_SRC ocaml {:regexp\| let ident = [a-z]+

   ::

       let v = ident*|'A'
       (* error *| will be parsed as single token *)                 
       |}

       {:regexp|

       let ident = [a-z]+
       let v = ident* |'A'
       (* correct *)                    
        |}  

   #+END\_SRC This may be considered as a defect which may be fixed in
   next release


