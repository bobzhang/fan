==========
Lexer DDSL
==========


``lexer DDSL`` is just another backend for ocamllex, the nice
`ocamllex tutorial
<http://pllab.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamllex-tutorial/index.html>`_
would also help understand lexer DDSL.

-  Syntax

   lexer DDSL's syntax is similar to ocamllex.

   .. code-block:: ocaml

      #regexp{
      let name1 = r1 let name2 = r2 ...
      let name n = rn
      };; 
      
      %lexer{
      | r1 -> e1
      | r2 -> e2
      | r3 -> e3
      } (* shortest = false *)


     %lexer{
      < r1 -> e1
      | r2 ->e2
      | r3 -> e3
      |} (* shortest = true *) 

Here r\ :sub:`n` stands for regex expression, e\ :sub`n` means Fan's
expression, different lexer DDSLs are guaranteed to be independent.

Similar to ocamllex, /lexbuf/ is available and captured in ``actions``

Keep in mind, ``lexer DDSL`` is simply an expression, it works with any
feature provided by OCaml. That said, in the action, you can start
another lexer as you want or you may put the lexer DDSL as a body of a
method definition.

The syntax for regex is the same as ocamllex, it supports all features
provided by ocamllex except two aspects:

   #. ``!`` stands end of file instead of ``eof`` in ocamllex
   #. lexer DDSL's lexing convention is more restrict.

.. warning::
   
   There is a minor bug currently for example:

   .. code-block:: ocaml

      #regexp{
      let ident = [a-z]+
      let v = ident*|'A'
      };;
      (* ERROR *| will be parsed as single token *)                 


      #regexp{
      let ident = [a-z]+
      let v = ident* |'A'
      };;  



