================
Toplevel support
================

.. highlight:: sh

-  Playing with toplevel

   If you have Fan installed successfully, start your toplevel, and try the
   next phrase:

   .. code-block:: ocaml

      #directory "+fan";;
      # #load "fanTop.cma";;
      # let a = %exp-{|3 };; 
      val a : FAstN.exp = \`Int "3"

   Here ``exp-`` is a built-in DDSL for quasiquotation, it's the same as ``exp``
   DDSL except without locations, see :doc:`quotation_system`


   There are two directives added:

      #. normal 
         
         .. code-block:: ocaml
            
            #normal;;
         
         This directive would restore the toplevel to the default
         behavior, it's useful sometimes, for examlpe, you want to load a
         normal ocaml file instead of file pre-processed by Fan.
      #. fan

         .. code-block:: ocaml
            
            #fan;;
            
         It will turn on the features of fan.

   .. warning::
      
      Don't fire the toplevel in the sourceree, as you may notice,
      there's a ``.ocamlinit`` file there which requires a customized
      toplevel, so fire the toplevel in any other place you like.

-  Playing with `Utop`_ 

   Utop is a toplevel with nice auto-completion support, it's very helpful for
   explore some libraries which the user is not familiar with.

   .. todo::

   There is adapter for fan, namely ftop, available here: https://github.com/bobzhang/ftop

   To install ftop::
     
     $ make clean 
     $ make
     $ make install

   Then you would have ``ftop`` in your path. Since ftop already linked fan in,
   so it works out of the box.

   .. code-block:: ocaml

      ftop$ let a = %exp-{ 3 };;
      val a: FAstN.exp = `Int "3"
   
   
   However, you still have to type :code:`"#directory "+fan"` to get
   access to Fan's typing environment /i.e./ cmi files.

.. include:: link_names.txt
