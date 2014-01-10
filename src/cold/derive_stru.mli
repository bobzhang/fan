
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

(* open Ctyp *)

type default =
  | Atom of exp
  | Invalid_argument 

type param = {
    arity: int;
    names: string list;
    plugin_name:  string;
    id: Ctyp.basic_id_transform;
    default: default option;
    mk_record: (Ctyp.record_col list -> exp) option;
    mk_variant: (string option -> Ctyp.ty_info list -> exp) option ;
    annot: (string -> (ctyp*ctyp)) option;
    builtin_tbl : (ctyp *  exp) list;
    excludes : string list;
  }


module type S =
  sig
    val p : param 
  end

module Make(U:S) : sig

  (**
     suppose [id] is [`Pre "xx"];
     [names] is [["arg0"; "arg1"]]
     {[
     [int] -> [xx int]
     [int list] -> [xxlist xxint]
     ]}
     For tuple type, it's more complicated.

     Two problems: bad eror message when
     %fn@sexp_of{'a list}

   *)
  val normal_simple_exp_of_ctyp : ctyp -> exp

      
  val exp_of_ctyp :  or_ctyp -> exp

  (** The first argument is type annotations.
      This function is used to handle polymorphic variant,
      it should be treated with special care
   *)    
  val exp_of_variant : ctyp -> row_field -> exp

  (** 1. idem
      
   *)
  val fun_of_tydcl : ctyp -> decl -> exp

  val bind_of_tydcl : decl -> bind

  val stru_of_mtyps : Sigs_util.mtyps -> stru option

end

val register : param -> unit    
