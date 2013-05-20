open LibUtil
open Ast
open AstLib


  
let meta = object
  inherit FanMeta.meta
  method! loc _loc  _ = lid _loc "loc"
end;;

    
AstFilters.register_stru_filter
("lift",
 (fun ast ->
   let _loc = loc_of ast in
   let e = (meta#stru _loc ast :ep  :> exp )in
   {:stru| let loc = FanLoc.ghost in $e |}));;


AstFilters.register_stru_filter ("strip",(new Objs.reloc  FanLoc.ghost)#stru);;


let map_exp = with exp function
  | {| $e NOTHING |} | {| function | NOTHING  -> $e |} -> e
  | {| __FILE__ |} -> {| $(`str:FanLoc.file_name _loc) |}
  | {| __PWD__ |} ->
      {|$(`str:Filename.dirname (FanLoc.file_name _loc) ) |}
  | {| __LOCATION__ |} ->
      let (a, b, c, d, e, f, g, h) = FanLoc.to_tuple _loc in
      {| FanLoc.of_tuple
        ($`str:a, $`int:b, $`int:c, $`int:d,
         $`int:e, $`int:f, $`int:g,
         $(if h then {| true |} else {| false |} )) |}
  | e -> e ;;

AstFilters.register_stru_filter ("trash_nothing",(Objs.map_exp map_exp)#stru);;
  
(* [s] should starts with __ *)
let make_filter (s,code) =
  let f =  function
    | {:stru| $lid:s'|} when s =s' ->
        FanAstN.fill_loc_stru _loc code
    | e -> e   in
  ("filter_"^s, (Objs.map_stru f )#stru)

let me = object
  inherit FanMeta.meta;
  method! loc _loc loc =
    match !AstQuotation.current_loc_name with
    | None -> lid _loc !FanLoc.name
    | Some "here" -> FanMeta.meta_loc _loc loc
    | Some x ->  lid  _loc x 
end

    
let mp = object
  inherit FanMeta.meta
  method! loc _loc _ = {:pat'| _ |} (* we use [subst_first_loc] *)    
end;;


  
AstFilters.register_stru_filter
    ("serialize",
     (fun x ->
        let _loc = FanLoc.ghost in 
        let y = (me#stru _loc x : ep :> exp)in 
        {:stru| $x ;; let __fan_repr_of_file = $y |}
        ) );;
