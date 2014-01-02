
(** Experimental: macro feature*)  

open Astf

(*
  %macro{M a b c}

  %macro{fib 32 }

  {:defmacro|fib a b =
  
  |}

  challegens lie in how to extract the
  macro name, and apply

  fib 32 -->
  fib 31 + fib 30

  -- `App
  --
  %stru{ g };

  -- macro.exp
  

  -- macro.stru
  `StExp ..  

  1. the position where macro quotation appears
  - clfield
  combine with translate, this should be inferred automatically


  2. the position where macro expander appears?
  currently only exp

  3. the type macro expander
  - macro.clfield should generate clfield 

  4. register

  5. dependency

  Guarantee:
  macro.exp should return exp
  macro.stru should return stru 
 *)

  
type key = string

type expander = exp -> exp


(*  exp -> exp *)
val macro_expanders : (key, expander) Hashtbl.t

val register_macro : key * expander -> unit

val fib : int -> int

(** {[%exp{ f a b c}
    don't support currying
    always f a
    or f (a,b,c)
    %exp{ f (a,b,c) } ]}*)
val fibm : exp -> exp

val macro_expander : Astf_map.map
