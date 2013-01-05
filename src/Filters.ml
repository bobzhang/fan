
open LibUtil;
(* open Ast; *)
module Ast = FanAst;
module MetaLoc = struct
   (* this makes sense here, because, for list operation
      you don't care about the location representation here
    *)
  let meta_loc_patt _loc _ = {:patt| loc |};
  let meta_loc_expr _loc _ = {:expr| loc |};
end;
module MetaAst = FanAst.Make MetaLoc;
AstFilters.register_str_item_filter ("lift",(fun ast ->
  let _loc = FanAst.loc_of_str_item ast in
  {:str_item| let loc = FanLoc.ghost in $(exp:MetaAst.Expr.meta_str_item _loc ast) |})); (* FIXME Loc => FanLoc*)

let add_debug_expr e =
  let _loc = FanAst.loc_of_expr e in
  let msg = "camlp4-debug: exc: %s at " ^ FanLoc.to_string _loc ^ "@." in
  {:expr|
      try $e  with
      [ XStream.Failure | Exit as exc -> raise exc
      | exc -> begin
          if Debug.mode "exc" then
            Format.eprintf $`str:msg (Printexc.to_string exc) else ();
          raise exc
        end ] |};

let rec map_match_case =
  fun
  [ {:match_case@_loc| $m1 | $m2 |} ->
      {:match_case| $(map_match_case m1) | $(map_match_case m2) |}
  | {:match_case@_loc| $pat:p when $w -> $e |} ->
      {:match_case@_loc| $pat:p when $w -> $(add_debug_expr e) |}
  | m -> m ];


AstFilters.register_str_item_filter ("exception",object
  inherit FanAst.map as super;
  method! expr = fun
  [ {:expr@_loc| fun [ $m ] |}  -> {:expr| fun [ $(map_match_case m) ] |}
  | x -> super#expr x ];
  method! str_item = fun
  [ {:str_item| module Debug = $_ |} as st -> st
  | st -> super#str_item st ];
end#str_item);

AstFilters.register_str_item_filter ("strip",(new FanAst.reloc  FanLoc.ghost)#str_item);

let decorate_binding decorate_fun = object
  inherit FanAst.map as super;
  method! binding = fun
    [ {:binding| $lid:id = $( ({:expr@_| fun [ $_ ] |} as e)) |} ->
      {:binding| $lid:id = $(decorate_fun id e) |}
    | b -> super#binding b ];
  end#binding;

let decorate decorate_fun = object (o)
  inherit FanAst.map as super;
  method! str_item = fun
    [ {:str_item@_loc| let $rec:r $b |} ->
      {:str_item| let $rec:r $(decorate_binding decorate_fun b) |}
    | st -> super#str_item st ];
  method! expr = fun
    [ {:expr@_loc| let $rec:r $b in $e |} ->
      {:expr| let $rec:r $(decorate_binding decorate_fun b) in $(o#expr e) |}
    | {:expr@_loc| fun [ $_ ] |} as e -> decorate_fun "<fun>" e
    | e -> super#expr e ];
end;

let decorate_this_expr e id =
  let buf = Buffer.create 42 in
  let _loc = FanAst.loc_of_expr e in
  let () = Format.bprintf buf "%s @@ %a@?" id FanLoc.dump _loc in
  let s = Buffer.contents buf in
  {:expr| let () = Camlp4prof.count $`str:s in $e |};

let rec decorate_fun id =
  let decorate = decorate decorate_fun in
  let decorate_expr = decorate#expr in
  let decorate_match_case = decorate#match_case in
  fun
  [ {:expr@_loc| fun $p -> $e |} ->
      {:expr| fun $p -> $(decorate_fun id e) |}
  | {:expr@_loc| fun [ $m ] |} ->
      decorate_this_expr {:expr| fun [ $(decorate_match_case m) ] |} id
  | e -> decorate_this_expr (decorate_expr e) id ];

AstFilters.register_str_item_filter("profile", (decorate decorate_fun)#str_item);
AstFilters.register_str_item_filter
    ("trash",(FanAst.map_str_item
      (fun
       [ {:str_item@_loc| module Camlp4Trash = $_ |} ->
            {:str_item||}
       | st -> st ]))#str_item);

let map_expr = with "expr" fun
  [ {| $e NOTHING |} | {| fun [NOTHING  -> $e] |} -> e
  | {| __FILE__ |} -> {| $(`str:FanLoc.file_name _loc) |}
  | {| __PWD__ |} ->
      {|$(`str:Filename.dirname (FanLoc.file_name _loc) ) |}
  | {| __LOCATION__ |} ->
      let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple _loc in
      {| FanLoc.of_tuple
        ($`str:a, $`int:b, $`int:c, $`int:d,
         $`int:e, $`int:f, $`int:g,
         $(if h then {| true |} else {| false |} )) |}
  | e -> e];

AstFilters.register_str_item_filter ("trash_nothing",(FanAst.map_expr map_expr)#str_item);
  
(* [s] should starts with "__" *)
let make_filter (s,code) =
  let f = with "str_item" fun
  [ {| $lid:s'|} when s =s' -> code
  | e -> e  ] in
  ("filter_"^s, (FanAst.map_str_item f )#str_item);

