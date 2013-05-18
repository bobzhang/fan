open Ast
  
val substp : loc -> (string * pat) list -> exp -> pat

class subst : loc -> (string * exp) list -> Objs.map

class type antiquot_filter =object
  inherit Objs.map
  method get_captured_variables: (exp * exp)list
  method clear_captured_variables: unit
end

val capture_antiquot: antiquot_filter

val filter_pat_with_captured_variables:  pat -> pat * (exp * exp) list

(**
  Given [args] and [body], generate an expession
  when [args] is nil, adding a [unit]
  Examples:
  {[
  fun_args _loc [{:pat|a|};{:pat|c|};{:pat|b|}] {|c|} |> FanBasic.p_exp f;
  fun a  c  b  -> c
  ]}
 *)    
val fun_args : loc -> pat list -> exp -> exp



(** Example: {[
  ["a";"b"] <+ {|3|};
  - : exp = fun a  b  -> 3
  ]} *)
val mkfun : string list -> exp -> exp

(** Example: {[
  [{:pat|a|}; {:pat|b|} ] <+< {|3|};
  - : exp = fun a  b  -> 3
  ]} *)  
val ( <+< ) : pat list -> exp -> exp
    
val mee_comma : exp -> exp -> exp
val mee_app : exp -> exp -> exp
(* val vee_of_str : string -> exp *)

(** {[
    mee_of_str "A" = {| {| A |}|};
    - : bool = true
  ]} Here [s] should be a capital alphabet *)
val mee_of_str : string -> exp


val mk_tuple_ee : exp list -> exp

val mee_record_col : string -> exp -> exp

val mee_record_semi : exp -> exp -> exp

val mk_record_ee : (string * exp) list -> exp

val eta_expand : exp -> int -> exp

val gen_curry_n : exp -> arity:int -> string -> int -> exp

val currying : case list -> arity:int -> exp

val unknown : int -> exp


(**  Example:
  {[mk_record [("a",{|3|});("b",{|4|})] ;
     - : exp = { a = 3; b = 4 }  ]}
  [mk_record] becomes a bit complex when you have to consider
  the arity
 *)    
val mk_record : (string * exp) list -> exp    


