open Format

open LibUtil

open AstLoc

open FSig

let transform =
  let _loc = FanLoc.ghost in
  let open Id in
    function
    | `Pre pre ->
        (fun x  -> (`Id (_loc, (ident_map (fun x  -> pre ^ x) x)) : Ast.exp ))
    | `Post post ->
        (fun x  ->
           (`Id (_loc, (ident_map (fun x  -> x ^ post) x)) : Ast.exp ))
    | `Fun f -> (fun x  -> (`Id (_loc, (ident_map f x)) : Ast.exp ))
    | `Last f ->
        (fun x  -> (`Id (_loc, (ident_map_of_ident f x)) : Ast.exp ))
    | `Id f -> (fun x  -> (`Id (_loc, (f x)) : Ast.exp ))
    | `Idents f ->
        (fun x  -> (`Id (_loc, (f (list_of_dot x []))) : Ast.exp ))
    | `Obj f ->
        (function
         | `Lid (_loc,x) ->
             (`Send
                (_loc, (`Id (_loc, (`Lid (_loc, "self")))),
                  (`Lid (_loc, (f x)))) : Ast.exp )
         | t ->
             let dest = map_to_string t in
             let src = Objs.dump_ident t in
             (if not (Hashtbl.mem Basic.conversion_table src)
              then
                (Hashtbl.add Basic.conversion_table src dest;
                 eprintf "Warning:  %s ==>  %s ==> unknown\n" src dest)
              else ();
              (`Send
                 (_loc, (`Id (_loc, (`Lid (_loc, "self")))),
                   (`Lid (_loc, (f dest)))) : Ast.exp )))

let basic_transform =
  function
  | `Pre pre -> (fun x  -> pre ^ x)
  | `Post post -> (fun x  -> x ^ post)
  | `Fun f -> f

let right_transform =
  let _loc = FanLoc.ghost in
  function
  | #basic_id_transform as x ->
      let f = basic_transform x in
      (fun x  -> `Id (_loc, (`Lid (_loc, (f x)))))
  | `Exp f -> f