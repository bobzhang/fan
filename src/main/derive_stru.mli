
(* (\** *)
(*    @supported types type application: list int *)
(*    basic type: int *)
(*    product type: (int * float * int) *)
(*    [m_list *)
(*    (fun _loc fmt ((a0, a1, a2), (b0, b1, b2)) -> *)
(*    ((m_int _loc fmt (a0, b0)), (m_float _loc fmt (a0, b0)), *)
(*    (m_float _loc fmt (a0, b0))))] *)
(*    return type is result *)
(*    Plz supply current type [type 'a list] =>  [list] *\)     *)
(* val normal_simple_exp_of_ctyp : *)
(*   ?arity:int -> *)
(*   ?names:string list -> *)
(*   mk_tuple:(ty_info list -> exp) -> *)
(*   right_type_id:full_id_transform -> *)
(*   left_type_id:basic_id_transform -> *)
(*   right_type_variable:rhs_basic_id_transform -> *)
(*   string Hashset.t -> ctyp -> exp *)

(*
  [default] is unnecessary for some type such as
  {[
  type u = A
  let cmp_u (x,y)= 
    match (x,y) with
    | (A, A) -> true
    |  _ -> false 
  ]}
 *)
open Astfn
open Sigs_util
open Ctyp



val mk :
    ?arity:int ->
      ?default:exp ->
        ?annot:(string -> (ctyp*ctyp)) ->
          id:basic_id_transform ->
            ?names:string list ->
              mk_record:(record_col list -> exp) ->
                mk_variant:(string option -> ty_info list -> exp) -> unit ->
                  mtyps -> stru
