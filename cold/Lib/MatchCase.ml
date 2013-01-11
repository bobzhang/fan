open LibUtil
open Ast
open Basic
let gen_tuple_abbrev ~arity  annot name e =
  let args: patt list =
    List.init arity
      (fun i  -> `Alias (_loc, (`PaTyp (_loc, name)), (xid ~off:i 0))) in
  let exps = List.init arity (fun i  -> `Id (_loc, (xid ~off:i 0))) in
  let e = Expr.apply e exps in
  let pat = args |> Patt.tuple_of_list in
  `Case (_loc, pat, (`Nil _loc), (`ExCoe (_loc, e, (`Nil _loc), annot)))