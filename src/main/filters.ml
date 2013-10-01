open LibUtil
open FAst
open AstLib


  
let meta = object
  inherit FMeta.meta
  method! loc _loc  _ = lid _loc "loc"
end;;

    
Ast_filters.register_stru_filter
("lift",
 (fun ast ->
   let _loc = loc_of ast in
   let e = (meta#stru _loc ast :ep  :> exp )in
   {:stru| let loc = FLoc.ghost in $e |}));;


Ast_filters.register_stru_filter ("strip",(new Objs.reloc  FLoc.ghost)#stru);;


let map_exp = with exp function
  | {| $e NOTHING |} | {| function | NOTHING  -> $e |} -> e
  | {| __FILE__ |} -> {| $(`str:FLoc.file_name _loc) |}
  | {| __PWD__ |} ->
      {|$(`str:Filename.dirname (FLoc.file_name _loc) ) |}
  | {| __LOCATION__ |} ->
      AstLib.meta_here _loc _loc
  | e -> e ;;

Ast_filters.register_stru_filter ("trash_nothing",(Objs.map_exp map_exp)#stru);;
  
(* [s] should starts with __ *)
let make_filter (s,code) =
  let f =  function
    | {:stru| $lid:s'|} when s =s' ->
        FanAstN.fill_stru _loc code
    | e -> e   in
  ("filter_"^s, (Objs.map_stru f )#stru)

let me = object
  inherit FMeta.meta;
  method! loc _loc loc =
    match !Ast_quotation.current_loc_name with
    | None -> lid _loc !FLoc.name
    | Some "here" ->
        meta_here _loc loc
    | Some x ->  lid  _loc x 
end

    
let mp = object
  inherit FMeta.meta
  method! loc _loc _ = {:pat'| _ |} (* we use [subst_first_loc] *)    
end;;


  
Ast_filters.register_stru_filter
    ("serialize",
     (fun x ->
        let _loc = FLoc.ghost in 
        let y = (me#stru _loc x : ep :> exp)in 
        {:stru| $x ;; let __fan_repr_of_file = $y |}
        ) );;
