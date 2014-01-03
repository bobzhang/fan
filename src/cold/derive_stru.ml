open Util
open Astn_util
open Astfn
open Ctyp
type param =  {
  arity: int;
  names: string list} 
let rec normal_simple_exp_of_ctyp ?arity  ?names  ~mk_tuple  ~right_type_id 
  ~left_type_id  ~right_type_variable  cxt (ty : ctyp) =
  let right_trans = transform right_type_id in
  let left_trans = basic_transform left_type_id in
  let tyvar = right_transform right_type_variable in
  let rec aux =
    function
    | `Lid id ->
        if Hashset.mem cxt id
        then lid (left_trans id)
        else right_trans (lid id)
    | #ident' as id -> right_trans (Idn_util.to_vid id)
    | `App (t1,t2) ->
        (`App ((aux t1 :>Astfn.exp), (aux t2 :>Astfn.exp)) :>Astfn.exp)
    | `Quote (_,`Lid s) -> tyvar s
    | `Arrow (t1,t2) ->
        aux
          (`App
             ((`App ((`Lid "arrow"), (t1 :>Astfn.ctyp))), (t2 :>Astfn.ctyp)) :>
          Astfn.ctyp)
    | `Par _ as ty ->
        tuple_exp_of_ctyp ?arity ?names ~mk_tuple
          ~f:(normal_simple_exp_of_ctyp ?arity ?names ~mk_tuple
                ~right_type_id ~left_type_id ~right_type_variable cxt) ty
    | (ty : ctyp) ->
        failwithf "normal_simple_exp_of_ctyp : %s" (Astfn_print.dump_ctyp ty) in
  aux ty
