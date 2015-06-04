=============
Fan's plugins
=============


-  A native example

   Suppose we want a new syntax for expressions,
   
   .. code-block:: ocaml

      (matches <pattern> ) or (matches when <pattern> when <expr>))
   
   that desugars into a test function returning a boolean:

   .. code-block:: ocaml
                   
      (function | <pattern> when <expr> -> true | _ -> false)


   Then we could create a file  as follows:
   
   .. code-block:: ocaml

      %create{ p};;

      %extend{
        p:
        [ pat{p};"when"; exp{e} ->
           %exp{ function | $p when $e -> true |_ -> false }
        | pat{p} -> %exp{ function | $p -> true | _ -> false } ]
      };;

      let d = `Absolute["Tutorial"];;
      AstQuotation.of_exp ~name:(d,"q") ~entry:p;;


   #. Create a new entry ``p`` for this DDSL
   #. Write the grammar for the new DDSL
   #. Register the new DDSL  named q, in the namespace ``Tutorial``.

.. highlight:: sh

   To compile the file::

     $ ocamlc -pp 'fan.native' testq.ml

- Test on the fly

  We can eval the code on the fly without installing the plugins using the
  preprocessor ``fane.byte``

  As follows:

  .. code-block:: ocaml

     #{:eval|
     open  Fsyntax
     {:create| Fgram p|};;

     {:extend|
       p:
       [pat{p};"when"; exp{e} -> {:exp| function | $pat:p when $e -> true
       |_ -> false |}
       |pat{p} -> {:exp| function | $pat:p -> true | _ -> false |} ]
     |};;
     let d = `Absolute["Tutorial"];;
     AstQuotation.of_exp ~name:(d,"q") ~entry:p;;
     |}  


     #{:control| import Tutorial ; |}
     if {:q| {:exp-| $a+$b|}|} {:exp-| 3 + 4|} then
         print_endline "yes!"
     else print_endline "no"



  To compile the file, you only need to switch from the preprocessor
  ``fan.byte`` to ``fane.byte``::

    $ ocamlc -pp 'fane.byte ' langQ.ml && ./a.out
