#default_quotation "match_case";;
open LibUtil;




open FanAst;
open Basic;
(*
  An ad-hoc solution for [`a|a|`b] code generation, to imporove later
 *)
let gen_tuple_abbrev  ~arity ~annot ~destination name e  =
  (* let annot = Ctyp.mk_dest_type *)
  let args : list patt =
    List.init arity (fun i ->
       (* `Alias (_loc, (`PaTyp (_loc, name)), (xid ~off:i 0 (\* :> ident *\)))) (\* FIXME *\) *)
      {:patt| (#$id:name as $(lid: x ~off:i 0 )) |})in
  let exps = List.init arity (fun i -> {:expr| $(id:xid ~off:i 0) |} ) in
  let e = (* Expr.apply *)appl_of_list [e:: exps] in 
  let pat = args |>tuple_com in
  let open FSig in
  match destination with
  [Obj(Map) ->
     {| $pat:pat -> ( $e : $id:name :> $annot) |}
  |_ ->
      {| $pat:pat -> ( $e  :> $annot) |}
  ]
    ;  
 


(* {:patt| (#$id:x as y)|} *)

















