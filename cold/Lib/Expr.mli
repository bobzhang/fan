open Ast
  
(* val sep_dot_exp : *)
(*   (loc * string list * exp) list -> *)
(*   exp -> (loc * string list * exp) list *)

(* val mksequence : ?loc:loc -> exp -> exp *)

(* val mksequence' : ?loc:loc -> exp -> exp *)

(* val mkassert : loc -> exp -> exp *)

(* val bigarray_get : loc -> exp -> exp -> exp *)

(* val bigarray_set : loc -> exp -> exp -> exp option *)

val pattern_eq_expression : patt -> exp -> bool

val map : loc -> patt -> exp -> exp -> exp

val filter : loc -> patt -> exp -> exp -> exp

val concat : loc -> exp -> exp

val compr :
  loc ->
  exp ->
  [> `cond of exp | `gen of patt * exp ] list -> exp

val bad_patt : FanLoc.t -> 'a

val substp : loc -> (string * patt) list -> exp -> patt

class subst: loc -> (string * exp) list -> Objs.map

class type antiquot_filter =object
  inherit Objs.map
  method get_captured_variables: (exp * exp)list
  method clear_captured_variables: unit
end

val capture_antiquot: antiquot_filter

val filter_patt_with_captured_variables:  patt -> patt * (exp * exp) list

val fun_args : loc -> patt list -> exp -> exp


(* val mkumin : loc -> string -> exp -> exp *)
(* val mk_assert : exp -> exp *)
val mk_record : (string * exp) list -> exp
val failure : exp
val ( <+ ) : string list -> exp -> exp
val ( <+< ) : patt list -> exp -> exp
val mee_comma : exp -> exp -> exp
val mee_app : exp -> exp -> exp
val vee_of_str : string -> exp

val mee_of_str : string -> exp

val meee_of_str : string -> exp

val mk_tuple_ee : exp list -> exp

val mee_record_col : string -> exp -> exp

val mee_record_semi : exp -> exp -> exp

val mk_record_ee : (string * exp) list -> exp

val eta_expand : exp -> int -> exp

val gen_curry_n : exp -> arity:int -> string -> int -> exp

val currying : case list -> arity:int -> exp

val unknown : int -> exp

(* val of_vstr_number : string -> int  -> exp *)
(* val of_str : string -> exp *)
(* val of_ident_number : ident -> int -> exp *)
(* val ( +> ) : exp -> string list -> exp *)
(* val gen_tuple_first : number:int -> off:int -> exp *)
(* val gen_tuple_second : number:int -> off:int -> exp *)
(* val tuple_of_number : exp -> int -> exp *)
(* val gen_tuple_n : ?cons_transform:(string->string) -> arity:int -> string -> int -> exp *)


