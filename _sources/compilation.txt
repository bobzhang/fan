==================================
Command Line compilation Using Fan
==================================

.. highlight:: sh

-  Hello world 
   
   Create a file ``hello.ml`` as follows:

   .. code-block:: ocaml
      
      print_endline "hello, Fan"

   The compile is quite simple, make sure ``fan.byte`` or
   ``fan.native`` is in your search path::

      $ ocamlc -pp 'fan.native' hello.ml -o test $ ./test

   As you may notice, adding ``-pp 'fan.native'`` flag is enough to
   switching to ``Fan``. Using ``fan.byte`` or ``fan.native`` is up to
   you, for the time being, only the performance matters here. So,
   compiling with the following command line does also work::

     $ ocamlc -pp 'fan.native' hello.ml -o test

-  First class lexer

   Writing hello world is not very interesting, for the following
   example, we show you how DDSL fits into Fan. Suppose we want to
   write a lexical analyzier to filter nested comments in OCaml, the
   traditional way is to write a complex regex expression, or start a
   new file to write a lexer.  The first way is hackish, inefficient,
   unmaintainable in the long run while the second way is too heavy
   weight, since lexer generator is a standard alone external DDSL
   which introduces another staging phase.

   Within Fan, we show how easy it is now:
   
   .. code-block:: ocaml

      let depth = ref 0

      let rec f  = %lex{
       | "(*" -> comment lexbuf
       | '"' -> (print_char '"'; string lexbuf)
       | _ as c -> (print_char c; f lexbuf)
       | ! -> exit 0
      }
      and comment  = %lex{
       | "*)" ->
           if !depth = 0 then f lexbuf
           else begin
             decr depth;
             comment lexbuf
           end
       | "(*" -> incr depth
       | _ -> comment lexbuf
       | ! -> failwith "unterminated comment"
      }
      and string = %lex{
       | '"' -> (print_char '"'; f lexbuf)
       | _ as c -> (print_char c; string lexbuf)
       | ! -> failwith "unterminated string"
      }

      let _ = f (Lexing.from_channel (open_in "comment.ml"));;  

   Compiling is the same as the previous example ``hello``::

        $ ocamlc -pp 'fan.native' comment.ml -o comment

   Here we see the lexer DDSL is first class construct in Fan, the
   user don't need to create a new file to isolate their lexer,
   it's as convenient as regex expression in perl. So it works in
   ``toplevel``, it works with module system, and objects, that
   said, the user could make lexer reusable by using `objects
   <http://caml.inria.fr/pub/docs/manual-ocaml/manual005.html>`_
   instead of functions.
      
   Abot the internal of ``lexer DDSL``, see :doc:`lexer`

