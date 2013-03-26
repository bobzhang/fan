open LibUtil;
open AstLoc;
let meta = object
  inherit FanMeta.meta;
  method! loc _loc  _ = `Id(_loc,`Lid(_loc,"loc"));
end;
AstFilters.register_stru_filter ("lift",(fun ast ->
  let _loc = loc_of ast in
  let e = (meta#stru _loc ast :ep  :> exp )in
  {:stru| let loc = FanLoc.ghost in $e |})); 


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
  method! exp = fun
    [ {:exp@_loc| let $rec:r $b in $e |} ->
      {:exp| let $rec:r $(decorate_binding decorate_fun b) in $(o#exp e) |}
    | {:exp@_loc| fun [ $_ ] |} as e -> decorate_fun "<fun>" e
    | e -> super#exp e ];
end;

let decorate_this_exp e id =
  let buf = Buffer.create 42 in
  let _loc = loc_of e in
  let () = Format.bprintf buf "%s @@ %a@?" id FanLoc.dump _loc in
  let s = Buffer.contents buf in
  {:exp| let () = Camlp4prof.count $`str:s in $e |};

let rec decorate_fun id =
  let decorate = decorate decorate_fun in
  let decorate_exp = decorate#exp in
  let decorate_case = decorate#case in
  fun
  [ {:exp@_loc| fun $p -> $e |} ->
      {:exp| fun $p -> $(decorate_fun id e) |}
  | {:exp@_loc| fun [ $m ] |} ->
      decorate_this_exp {:exp| fun [ $(decorate_case m) ] |} id
  | e -> decorate_this_exp (decorate_exp e) id ];

AstFilters.register_stru_filter("profile", (decorate decorate_fun)#stru);

let map_exp = with exp fun
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

AstFilters.register_stru_filter ("trash_nothing",(FanObjs.map_exp map_exp)#stru);
  
(* [s] should starts with __ *)
let make_filter (s,code) =
  let f = with stru fun
  [ {| $lid:s'|} when s =s' -> code
  | e -> e  ] in
  ("filter_"^s, (FanObjs.map_stru f )#stru);

let me = object
  inherit FanMeta.meta;
  method! loc _loc loc =
    match !AstQuotation.current_loc_name with
    [ None -> {:exp| $(lid:!FanLoc.name) |}
    | Some "here" -> Lib.Meta.meta_loc _loc loc
    | Some x -> {:exp| $lid:x |} ];
end;
let mp = object
  inherit FanMeta.meta;
  method! loc _loc _ = {:pat| _ |}; (* we use [subst_first_loc] *)    
end;


  
AstFilters.register_stru_filter
    ("serialize",
     (fun x ->
        let _loc = FanLoc.ghost in 
        let y = (me#stru _loc x : ep :> exp)in 
        {:stru| $x; let __fan_repr_of_file = $y |}
        ) );  
