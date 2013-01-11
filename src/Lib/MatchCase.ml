#default_quotation "match_case";;
open LibUtil;
(* let _loc = FanLoc.ghost ; *)
open Ast;
open Basic;
(* FIXME move to match_case.ml *)
let gen_tuple_abbrev  ~arity annot name e (* n *) =
  let args : list patt =
    List.init arity (fun i ->
       `Alias (_loc, (`PaTyp (_loc, name)), (xid ~off:i 0 (* :> ident *)))) (* FIXME *)
      (* {:patt| (#$name as $(id:xid ~off:i 0))|})  *)in
  let exps = List.init arity (fun i -> {:expr| $(id:xid ~off:i 0) |} ) in
  let e = Expr.apply e exps in 
  let pat = args |> Patt.tuple_of_list in
  {| $pat:pat -> ( $e :> $annot) |};




















