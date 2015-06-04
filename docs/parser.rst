===========
Parser DDSL
===========


Fan's parser DDSL is fully self-explainable, its runtime has enough
meta-data to reconstruct itself.

-  Learn the syntax by yourself

   Suppose you have toplevel installed successfully ( see :doc:`toplevel`).

   .. code-block:: ocaml

      $ #directory "+fan";; 
      $ #load "fanTop.cma";;


   Now if you want to understand what's the definition for the syntactic
   category case
   
   .. code-block:: ocaml
                   
      Fgram.print Format.std_formatter Fsyntax.case ;;


   The output is something like this:

   .. code-block:: ocaml

      case: 
        [ LA 
        [ "|"; L1 case0 SEP "|" -> bar_of_list l
        | pat; "->"; exp -> `Case (_loc, p, e)]
        ]



   Unlike `Camlp4`_, such grammar printer is the original grammar which is
   100% honest to the user's input, no computation involved.

   So you can install the printer in the toplevel and explore different
   non-terminals by your self.

   .. code-block:: ocaml
      
      $ #install_printer Fgram.print;;
     

.. todo::
   
   -  How the parser works

   -  Where is the lexer?
   -  TODO Functional generation


.. include:: ./link_names.txt
