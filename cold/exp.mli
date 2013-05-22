open FAst


(** Environment is a [string*pat] pair,
   Try to convert the 
   [exp] node into [pat] node.
   when do the conversion, if the exp node has an identifier which
   has a special meaning, then that replacment will be used  *)  
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
(* val fun_args : loc -> pat list -> exp -> exp *)



(* (\** Example: {[ *)
(*   ["a";"b"] <+ {|3|}; *)
(*   - : exp = fun a  b  -> 3 *)
(*   ]} *\) *)
(* val mkfun : string list -> exp -> exp *)

(* (\** Example: {[ *)
(*   [{:pat|a|}; {:pat|b|} ] <+< {|3|}; *)
(*   - : exp = fun a  b  -> 3 *)
(*   ]} *\)   *)
(* val ( <+< ) : pat list -> exp -> exp *)
    

(* val mee_comma : exp -> exp -> exp *)

(* val mee_app : exp -> exp -> exp *)
(* (\* val vee_of_str : string -> exp *\) *)

(* (\** {[ *)
(*     mee_of_str "A" = {| {| A |}|}; *)
(*     - : bool = true *)
(*   ]} Here [s] should be a capital alphabet *\) *)
(* val mee_of_str : string -> exp *)



(* (\** *)
(*    @raise Invalid_argument *)
   
(*    There are 2 stages here  *)
(*    We want to generate code  like this *)
(*    {[ *)

(*     {|  {| ( $(meta_int _loc x0), $(meta_int _loc x1) ) |}  |} *)
(*    ]} *)

(*   Normal practice: *)
(*   First print the result, then find a mechanical way to   construct *)

(*   Here we should avoid singleton tuple error *)
(*   {| $par:a |} when a is  single, it will cause error FIXME *)

(*  *\)       *)

  
(* (\** *)
(*   Here we want to generate something like *)
  
(*   {[ *)
(*   ({| {| { u = $meta } |} |} ); *)
(*   ]} *)
(*   [meta] could be parameterized *)
  
(*   First we need to construct this part *)
(*   {[ *)
(*   (App  *)
(*        (App  (ExId  (IdAcc  (Uid  "FAst") (Uid  "RbEq"))) *)
(*          (ExId  (Lid  "_loc"))) *)
(*        (App  *)
(*          (App  (ExId  (IdAcc  (Uid  "FAst") (Uid  "Lid"))) *)
(*            (ExId  (Lid  "_loc"))) *)
(*          (Str  "u"))) *)
(*   ]} *)
(*   given string input u *)
(*   we finally want to make  *)
(*   {[ *)
(*   {| << {u = $meta_u$ ; v = $meta_v$ } |} >>  *)
(*   ]} *)
(*   given string input "u" and [ {| meta_u |} ] *\) *)
(* val mk_tuple_ee : exp list -> exp *)

(* (\**  Example:  {[ *)
(*   mee_record_col "a" {|3|} = {| {:rec_exp| a = $($({|3|})) |}|}; *)
(*   ]} *\) *)
(* val mee_record_col : string -> exp -> exp *)

(* val mee_record_semi : exp -> exp -> exp *)

(* (\**  Example:  {[ *)
(*      mk_record_ee [("a",{|3|})] = {| {| { a = $($({|3|})) }|}|}; *)
(*      ]}*\) *)
(* val mk_record_ee : (string * exp) list -> exp *)

(* (\** Mainly used to overcome the value restriction {[ *)
(*     eta_expand {|f |} 3 |> FanBasic.p_exp f; *)
(*     fun _a0  _a1  _a2  -> f _a0 _a1 _a2 *)
(*    ]} *\) *)

(* val eta_expand : exp -> int -> exp *)


(* (\**  Example:  {[ *)
(*      gen_curry_n {|3|} ~arity:2 "`X" 2 ; *)
(*      fun [ `X (a0, a1) -> fun [ `X (b0, b1) -> 3 ] ] *)
(*      gen_curry_n {|3|} ~arity:2 "X" 2 ; *)
(*      fun (X (a0,a1))  (X (b0,b1))  -> 3 *)
(*   ]} *\) *)
(* val gen_curry_n : exp -> arity:int -> string -> int -> exp *)


(* (\** Example: {[ *)
(*    let u  =  list_of_or' {:case| *)
(*   (A0 (a0, a1),A0 (b0, b1)) -> 1 *)
(*   |   (A1 (a0, a1), A1 (b0, b1)) -> 2 *)
(*   |   (A2 (a0, a1), A2 (b0, b1)) -> 3 |} [] in currying ~arity:2 u ; *)

(*   fun a0  b0  -> *)
(*   match (a0, b0) with *)
(*   | (A0 (a0,a1),A0 (b0,b1)) -> 1 *)
(*   | (A1 (a0,a1),A1 (b0,b1)) -> 2 *)
(*   | (A2 (a0,a1),A2 (b0,b1)) -> 3  ]} *)
(*   Make Sure the names generated are shadowed by *)
(*   gen_tuple_n *)
(*  (\* FIXME when cases is []*\)   *)
(*  *\) *)
(* val currying : case list -> arity:int -> exp *)

(* val unknown : int -> exp *)


(* (\**  Example: *)
(*   {[mk_record [("a",{|3|});("b",{|4|})] ; *)
(*      - : exp = { a = 3; b = 4 }  ]} *)
(*   [mk_record] becomes a bit complex when you have to consider *)
(*   the arity *)
(*  *\)     *)
(* val mk_record : (string * exp) list -> exp     *)


