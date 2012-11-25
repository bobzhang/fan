
open lang "patt";
open Camlp4Ast;
module Ast = Camlp4Ast; (* it has a nested  Ast module, FIXME *)


let mklist _loc =
  let rec loop top = fun
    [ [] -> {:patt| [] |}
    | [p1 :: pl] ->
        let _loc =
          if top then _loc else FanLoc.merge (loc_of_patt p1) _loc in
        {:patt| [$p1 :: $(loop false pl)] |} ] in loop true ;


let tuple _loc  =   fun
  [[] -> {|()|}
  |[p] -> p
  | [e::es] -> {| ($e, $list:es) |} ];

