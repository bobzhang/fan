(** Ast Utilities for [FAstN.expN] *)
open FAstN
  


(** {[ mkfun ["a";"b"]  %{3};
      - : exp = fun a  b  -> 3  ]} *)
val mkfun : string list -> exp -> exp


(**

  Example:
  {[
   let u  =  list_of_bar {:case|
   (A0 (a0, a1),A0 (b0, b1)) -> 1
   |   (A1 (a0, a1), A1 (b0, b1)) -> 2
   |   (A2 (a0, a1), A2 (b0, b1)) -> 3 |} [] in currying ~arity:2 u ;

  fun _a0  _b0  ->
  match (_a0, _b0) with
  | (A0 (a0,a1),A0 (b0,b1)) -> 1
  | (A1 (a0,a1),A1 (b0,b1)) -> 2
  | (A2 (a0,a1),A2 (b0,b1)) -> 3
  ]}
  Make Sure the names generated are shadowed by
  [gen_tuple_n]
  (* FIXME when cases is []*)
 *)

val currying : case list -> arity:int -> exp


    
val unknown : int -> exp    

(** Mainly used to overcome the value restriction
   {[
    eta_expand %{f } 3 |> FanBasic.p_exp f;
    fun _a0  _a1  _a2  -> f _a0 _a1 _a2   ]} *)
val eta_expand : exp -> int -> exp    

val mk_record : (string * exp) list -> exp
    
(** meta level code generation *)
val mee_comma : exp -> exp -> exp
val mee_app : exp -> exp -> exp
val mee_of_str : string -> exp
val mk_tuple_ee : exp list -> exp
val mee_record_col : string -> exp -> exp
val mee_record_semi : exp -> exp -> exp
val mk_record_ee : (string * exp) list -> exp
    
