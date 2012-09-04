module Make :
 functor (Camlp4Ast : Sig.Camlp4Ast) ->
  sig
   open Camlp4Ast

   val patt : (patt -> Parsetree.pattern)

   val sig_item : (sig_item -> Parsetree.signature)

   val str_item : (str_item -> Parsetree.structure)

   val phrase : (str_item -> Parsetree.toplevel_phrase)

  end
