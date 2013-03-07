open Ast
  
(* val sep_dot_expr : *)
(*   (loc * string list * expr) list -> *)
(*   expr -> (loc * string list * expr) list *)

(* val mksequence : ?loc:loc -> expr -> expr *)

(* val mksequence' : ?loc:loc -> expr -> expr *)

(* val mkassert : loc -> expr -> expr *)

(* val bigarray_get : loc -> expr -> expr -> expr *)

(* val bigarray_set : loc -> expr -> expr -> expr option *)

val pattern_eq_expression : patt -> expr -> bool

val map : loc -> patt -> expr -> expr -> expr

val filter : loc -> patt -> expr -> expr -> expr

val concat : loc -> expr -> expr

val compr :
  loc ->
  expr ->
  [> `cond of expr | `gen of patt * expr ] list -> expr

val bad_patt : FanLoc.t -> 'a

val substp : loc -> (string * patt) list -> expr -> patt

class subst: loc -> (string * expr) list -> Objs.map

class type antiquot_filter =object
  inherit Objs.map
  method get_captured_variables: (expr * expr)list
  method clear_captured_variables: unit
end

val capture_antiquot: antiquot_filter

val filter_patt_with_captured_variables:  patt -> patt * (expr * expr) list

val fun_args : loc -> patt list -> expr -> expr


(* val mkumin : loc -> string -> expr -> expr *)
(* val mk_assert : expr -> expr *)
val mk_record : (string * expr) list -> expr
val failure : expr
val ( <+ ) : string list -> expr -> expr
val ( <+< ) : patt list -> expr -> expr
val mee_comma : expr -> expr -> expr
val mee_app : expr -> expr -> expr
val vee_of_str : string -> expr

val mee_of_str : string -> expr

val meee_of_str : string -> expr

val mk_tuple_ee : expr list -> expr

val mee_record_col : string -> expr -> expr

val mee_record_semi : expr -> expr -> expr

val mk_record_ee : (string * expr) list -> expr

val eta_expand : expr -> int -> expr

val gen_curry_n : expr -> arity:int -> string -> int -> expr

val currying : match_case list -> arity:int -> expr

val unknown : int -> expr

(* val of_vstr_number : string -> int  -> expr *)
(* val of_str : string -> expr *)
(* val of_ident_number : ident -> int -> expr *)
(* val ( +> ) : expr -> string list -> expr *)
(* val gen_tuple_first : number:int -> off:int -> expr *)
(* val gen_tuple_second : number:int -> off:int -> expr *)
(* val tuple_of_number : expr -> int -> expr *)
(* val gen_tuple_n : ?cons_transform:(string->string) -> arity:int -> string -> int -> expr *)


