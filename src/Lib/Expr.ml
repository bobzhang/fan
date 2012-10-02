open Camlp4Ast;


(*
  {[
  sep_expr_acc <:expr< A.B.g.h >>

  [(, ["A"; "B"], Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "g")));
  (, [], Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "h")))]

  The first two dots are IdAcc, the last dot is ExAcc

  sep_expr_acc <:expr< A.B.g.h + 3 >>

  [(, [],
  Camlp4Ast.Ast.ExApp (,
   Camlp4Ast.Ast.ExApp (, Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "+")),
    Camlp4Ast.Ast.ExAcc (,
     Camlp4Ast.Ast.ExId (,
      Camlp4Ast.Ast.IdAcc (, Camlp4Ast.Ast.IdUid (, "A"),
       Camlp4Ast.Ast.IdAcc (, Camlp4Ast.Ast.IdUid (, "B"),
        Camlp4Ast.Ast.IdLid (, "g")))),
     Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "h")))),
   Camlp4Ast.Ast.ExInt (, "3")))]

  sep_expr_acc <:expr< A.B.g.h.i>>
  [(, ["A"; "B"], Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "g")));
  (, [], Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "h")));
  (, [], Camlp4Ast.Ast.ExId (, Camlp4Ast.Ast.IdLid (, "i")))]
  ]}
 *)
let rec sep_expr_acc l = fun
  [ <:expr< $e1.$e2>> ->
    sep_expr_acc (sep_expr_acc l e2) e1
  | <:expr@loc< $uid:s >> as e ->
      match l with
      [ [] -> [(loc, [], e)]
      | [(loc', sl, e) :: l] -> [(FanLoc.merge loc loc', [s :: sl], e) :: l] ]
  | <:expr< $(id:(<:ident< $_.$_ >> as i)) >> ->
      sep_expr_acc l (Ident.normalize_acc i)
  | e -> [(loc_of_expr e, [], e) :: l] ];

