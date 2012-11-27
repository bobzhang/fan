open Camlp4Ast
module Ast = Camlp4Ast
let mklist _loc =
  let rec loop top =
    function
    | [] -> Ast.PaId (_loc, (Ast.IdUid (_loc, "[]")))
    | p1::pl ->
        let _loc = if top then _loc else FanLoc.merge (loc_of_patt p1) _loc in
        Ast.PaApp
          (_loc,
            (Ast.PaApp
               (_loc, (Ast.PaId (_loc, (Ast.IdUid (_loc, "::")))), p1)),
            (loop false pl)) in
  loop true
let tuple _loc =
  function
  | [] -> Ast.PaId (_loc, (Ast.IdUid (_loc, "()")))
  | p::[] -> p
  | e::es -> Ast.PaTup (_loc, (Ast.PaCom (_loc, e, (Ast.paCom_of_list es))))