#default_quotation "case";;



open LibUtil
open Ast
open AstLoc
open Basic


(*
  An ad-hoc solution for [`a|a|`b] code generation, to imporove later
 *)
let gen_tuple_abbrev  ~arity ~annot ~destination name e  =
  (* let annot = Ctyp.mk_dest_type *)
  let _loc =FanLoc.ghost in  
  let args :  pat list =

    List.init arity (fun i ->
      {:pat| (#$id:name as $(lid: x ~off:i 0 )) |})in
  let exps = List.init arity (fun i -> {:exp| $(id:xid ~off:i 0) |} ) in
  let e = appl_of_list (e:: exps) in 
  let pat = args |>tuple_com in

  let open FSig in
  match destination with
  | Obj(Map) ->
     {| $pat:pat -> ( $e : $((name:>ctyp)) :> $annot) |}
  |_ ->
      {| $pat:pat -> ( $e  :> $annot) |}

 


(* {:pat| (#$id:x as y)|} *)

















