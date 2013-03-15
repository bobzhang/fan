
open LibUtil;

open AstLoc;

(* module MetaLoc = struct *)
(*    (\* this makes sense here, because, for list operation *)
(*       you don't care about the location representation here *\) *)
(*   let meta_loc  _loc _ = `Id(_loc,`Lid(_loc,"loc")) (\* {:patt| loc |} *\); *)
(* end; *)
(* module MetaAst = FanAst.Make MetaLoc; *)
let meta = object
  inherit FanMeta.meta;
  method! loc _loc  _ = `Id(_loc,`Lid(_loc,"loc"));
end;
AstFilters.register_stru_filter ("lift",(fun ast ->
  let _loc = loc_of ast in
  let e = (meta#stru _loc ast :ep  :> expr )in
  {:stru| let loc = FanLoc.ghost in $e |}
  (* {:stru| let loc = FanLoc.ghost in $(exp:MetaAst.Expr.meta_stru _loc ast) |} *))); (* FIXME Loc => FanLoc*)

let add_debug_expr (e:expr) : expr =
  let _loc = loc_of e in
  let msg = "camlp4-debug: exc: %s at " ^ FanLoc.to_string _loc ^ "@." in
  {:expr|
      try $e  with
      [ XStream.Failure | Exit as exc -> raise exc (* FIXME *)
      | exc -> begin
          if Debug.mode "exc" then
            Format.eprintf $`str:msg (Printexc.to_string exc) else ();
          raise exc
        end ] |};

let rec map_case : case -> case  = with case 
  fun
  [ {| $m1 | $m2 |} ->
      {| $(map_case m1) | $(map_case m2) |}
  | {| $pat:p -> $e |} -> {|$pat:p -> $(add_debug_expr e)|}
  | {| $pat:p when $w -> $e |} ->
      {| $pat:p when $w -> $(add_debug_expr e) |}
  | m -> m ];


AstFilters.register_stru_filter ("exception",object
  inherit Objs.map as super;
  method! expr = fun
  [ {:expr@_loc| fun [ $m ] |}  -> {:expr| fun [ $(map_case m) ] |}
  | x -> super#expr x ];
  method! stru = fun
  [ {:stru| module Debug = $_ |} as st -> st
  | st -> super#stru st ];
end#stru);

AstFilters.register_stru_filter ("strip",(new FanObjs.reloc  FanLoc.ghost)#stru);

let decorate_binding decorate_fun = object
  inherit Objs.map as super;
  method! binding = fun
    [ `Bind (_loc,(`Id (_,`Lid (_,id)) as x),(`Fun (_,_) as e)) ->
      `Bind (_loc, x, (decorate_fun id e))
    | b -> super#binding b];
  end#binding;

let decorate decorate_fun = object (o)
  inherit Objs.map as super;
  method! stru = fun
    [ {:stru@_loc| let $rec:r $b |} ->
      {:stru| let $rec:r $(decorate_binding decorate_fun b) |}
    | st -> super#stru st ];
  method! expr = fun
    [ {:expr@_loc| let $rec:r $b in $e |} ->
      {:expr| let $rec:r $(decorate_binding decorate_fun b) in $(o#expr e) |}
    | {:expr@_loc| fun [ $_ ] |} as e -> decorate_fun "<fun>" e
    | e -> super#expr e ];
end;

let decorate_this_expr e id =
  let buf = Buffer.create 42 in
  let _loc = loc_of e in
  let () = Format.bprintf buf "%s @@ %a@?" id FanLoc.dump _loc in
  let s = Buffer.contents buf in
  {:expr| let () = Camlp4prof.count $`str:s in $e |};

let rec decorate_fun id =
  let decorate = decorate decorate_fun in
  let decorate_expr = decorate#expr in
  let decorate_case = decorate#case in
  fun
  [ {:expr@_loc| fun $p -> $e |} ->
      {:expr| fun $p -> $(decorate_fun id e) |}
  | {:expr@_loc| fun [ $m ] |} ->
      decorate_this_expr {:expr| fun [ $(decorate_case m) ] |} id
  | e -> decorate_this_expr (decorate_expr e) id ];

AstFilters.register_stru_filter("profile", (decorate decorate_fun)#stru);

let map_expr = with expr fun
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

AstFilters.register_stru_filter ("trash_nothing",(FanObjs.map_expr map_expr)#stru);
  
(* [s] should starts with __ *)
let make_filter (s,code) =
  let f = with stru fun
  [ {| $lid:s'|} when s =s' -> code
  | e -> e  ] in
  ("filter_"^s, (FanObjs.map_stru f )#stru);

(* module ME = FanAst.Make Ant.LocExpr; *)
(* module MP = FanAst.Make Ant.LocPatt; *)
let me = object
  inherit FanMeta.meta;
  method! loc _loc loc =
    match !AstQuotation.current_loc_name with
    [ None -> {:expr| $(lid:!FanLoc.name) |}
    | Some "here" -> Lib.Meta.meta_loc _loc loc
    | Some x -> {:expr| $lid:x |} ];
end;
let mp = object
  inherit FanMeta.meta;
  method! loc _loc _ = {:patt| _ |}; (* we use [subst_first_loc] *)    
end;


  
AstFilters.register_stru_filter
    ("serialize",
     (fun x ->
        let _loc = FanLoc.ghost in 
        let y = (me#stru _loc x : ep :> expr)in 
        {:stru| $x; let __fan_repr_of_file = $y |}
        ) );  
