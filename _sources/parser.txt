===========
Parser DDSL
===========


Fan's parser DDSL is fully self-explainable, its runtime has enough
meta-data to reconstruct itself.

-  Learn the syntax by yourself

suppose you have toplevel installed successfully ( see
file:./toplevel\_support.org).

| #+BEGIN\_SRC ocaml $ #directory "+fan";; $ #load "fanTop.cma";;
|  #+END\_SRC

Now if you want to understand what's the definition for the syntactic
category case

| #+BEGIN\_SRC ocaml Fgram.print Format.std\_formatter Fsyntax.case ;;
|  #+END\_SRC

The output is something like this: #+BEGIN\_EXAMPLE case: [ LA [ "\|";
L1 case0 SEP "\|" -> bar\_of\_list l

::

       | pat; "->"; exp -> `Case (_loc, p, e)
     ]]

#+END\_EXAMPLE

Unlike Camlp4, such grammar printer is the original grammar which is
100% honest to the user's input, no computation involved.

So you can install the printer in the toplevel and explore different
non-terminals by your self.

#+BEGIN\_SRC ocaml #install\_printer Fgram.print;; #+END\_SRC

-  How the parser works

-  Where is the lexer?
-  TODO Functional generation


