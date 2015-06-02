=============
Fan's plugins
=============


-  A native example

Suppose we want a new syntax for expressions,

#+BEGIN\_SRC ocaml (matches ) or (matches when ), #+END\_SRC that
desugars into a test function returning a boolean:

#+BEGIN\_SRC ocaml (function \| when -> true \| \_ -> false) #+END\_SRC

Then we could create a file [[file:code/testq.ml][testq]]

First, we need to create a new entry =p= for this DDSL

#+INCLUDE:"./code/testq.ml" src ocaml :lines "2-3"

After that we write the grammar for the new DDSL

#+INCLUDE:"./code/testq.ml" src ocaml :lines "4-8"

Then we start to register the new DDSL

#+INCLUDE:"./code/testq.ml" src ocaml :lines "10-12"

Yes, we are finished, we register a language named q, in the namespace
Tutorial.

To compile the file

#+BEGIN\_SRC sh ocamlc -pp 'fan.native' testq.ml #+END\_SRC

\*\* Test on the fly

we can eval the code on the fly without installing the plugins using the
preprocessor =fane.byte=

As follows:

#+INCLUDE:"./code/langQ.ml" src ocaml :lines "1-22"

To compile the file, you only need to switch from the preprocessor
fan.byte to fane.byte

#+BEGIN\_SRC sh ocamlc -pp 'fane.byte ' langQ.ml && ./a.out #+END\_SRC
