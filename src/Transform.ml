
open Format
open LibUtil
open Ast
open AstLib
open FSig
let transform :full_id_transform -> vid -> exp  =
  let _loc = FanLoc.ghost in
  let open Id in with exp function
    | `Pre pre ->
        fun  x ->  (ident_map (fun x -> pre ^ x) x : exp)
            (* fun [x -> {| $(id: ident_map (fun x ->  pre ^ x) x ) |} ] *)
    | `Post post ->
        fun x -> (ident_map (fun x-> x ^ post) x : exp )
            (* fun [x -> {| $(id:  ident_map (fun x -> x ^ post) x ) |} ] *)
    | `Fun f ->
        fun x -> ident_map f x 
            (* fun [x -> {| $(id:  ident_map f x ) |} ] *)
    | `Last f ->
        fun  x -> (ident_map_of_ident f x : vid :> exp)
            (* {| $(id: ident_map_of_ident f x  ) |} *) 
            
    | `Id f->
        fun x -> (f x : vid :> exp)
            (* fun [x -> {| $(id: f x ) |} ] *)
    | `Idents f ->
        fun x  -> (f (list_of_dot x []) : vid :> exp )
            (* fun [x -> {| $(id: f (list_of_dot x []) )  |}  ] *)
    | `Obj f ->
        function
          | `Lid(_loc,x)  -> {| self# $(lid: f x) |}
          | t -> 
              let dest =  map_to_string t in
              let src = Objs.dump_vid t in (* FIXME *)
              let () = if not (Hashtbl.mem Basic.conversion_table src) then begin 
                  Hashtbl.add Basic.conversion_table src dest;   
                  eprintf "Warning:  %s ==>  %s ==> unknown\n" src dest;
                end in
              {| self# $(lid:f dest) |}
                  (*todo  set its default let to self#unknown *)

let basic_transform = function 
  | `Pre pre -> (fun x -> pre ^ x)
  | `Post post -> (fun x -> x ^ post)
  | `Fun f -> f 
  
let right_transform =
  let _loc = FanLoc.ghost in with exp function
    | #basic_id_transform as x ->
        (** add as here to overcome the type system *)
        let f = basic_transform x in 
        fun x -> {| $(lid: f x) |} 
    | `Exp f -> f 
          




















