
(** Ast Utilities for [FAst.exp] *)
  
(* open FAst *)


(** Environment is a [string*pat] pair,
   Try to convert the 
   [exp] node into [pat] node.
   when do the conversion, if the exp node has an identifier which
   has a special meaning, then that replacment will be used  *)  
(* val substp : loc -> (string * pat) list -> exp -> pat *)

(* class subst : loc -> (string * exp) list -> Objs.map *)

(* class type antiquot_filter =object *)
(*   inherit Objs.map *)
(*   method get_captured_variables: (exp * exp)list *)
(*   method clear_captured_variables: unit *)
(* end *)

(* val capture_antiquot: antiquot_filter *)

(* val filter_pat_with_captured_variables:  pat -> pat * (exp * exp) list *)

