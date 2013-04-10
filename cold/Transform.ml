open Format

open LibUtil

open Ast

open AstLoc

open FSig

let transform: full_id_transform -> vid -> exp =
  let _loc = FanLoc.ghost in
  let open Id in
    function
    | `Pre pre -> (fun x  -> (ident_map (fun x  -> pre ^ x) x : exp ))
    | `Post post -> (fun x  -> (ident_map (fun x  -> x ^ post) x : exp ))
    | `Fun f -> (fun x  -> ident_map f x)
    | `Last f -> (fun x  -> (ident_map_of_ident f x : vid  :>exp))
    | `Id f -> (fun x  -> (f x : vid  :>exp))
    | `Idents f -> (fun x  -> (f (list_of_dot x []) : vid  :>exp))
    | `Obj f ->
        (function
         | `Lid (_loc,x) ->
             (`Send (_loc, (`Lid (_loc, "self")), (`Lid (_loc, (f x)))) : 
             Ast.exp )
         | t ->
             let dest = map_to_string t in
             let src = Objs.dump_vid t in
             (if not (Hashtbl.mem Basic.conversion_table src)
              then
                (Hashtbl.add Basic.conversion_table src dest;
                 eprintf "Warning:  %s ==>  %s ==> unknown\n" src dest)
              else ();
              (`Send (_loc, (`Lid (_loc, "self")), (`Lid (_loc, (f dest)))) : 
              Ast.exp )))

let basic_transform =
  function
  | `Pre pre -> (fun x  -> pre ^ x)
  | `Post post -> (fun x  -> x ^ post)
  | `Fun f -> f

let right_transform =
  let _loc = FanLoc.ghost in
  function
  | #basic_id_transform as x ->
      let f = basic_transform x in (fun x  -> `Lid (_loc, (f x)))
  | `Exp f -> f