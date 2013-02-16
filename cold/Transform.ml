open Format
open LibUtil
open Lib
open Ast
open FSig
let transform: full_id_transform -> ident -> expr =
  let _loc = FanLoc.ghost in
  let open Lib.Ident in
    function
    | `Pre pre -> (fun x  -> `Id (_loc, (ident_map (fun x  -> pre ^ x) x)))
    | `Post post ->
        (fun x  -> `Id (_loc, (ident_map (fun x  -> x ^ post) x)))
    | `Fun f -> (fun x  -> `Id (_loc, (ident_map f x)))
    | `Last f -> (fun x  -> `Id (_loc, (ident_map_of_ident f x)))
    | `Ident f -> (fun x  -> `Id (_loc, (f x)))
    | `Idents f -> (fun x  -> `Id (_loc, (f (list_of_acc_ident x []))))
    | `Obj f ->
        (function
         | `Lid (_loc,x) ->
             `Send
               (_loc, (`Id (_loc, (`Lid (_loc, "self")))),
                 (`Lid (_loc, (f x))))
         | t ->
             let dest = map_to_string t in
             let src = Objs.dump_ident t in
             (if not (Hashtbl.mem Basic.conversion_table src)
              then
                (Hashtbl.add Basic.conversion_table src dest;
                 eprintf "Warning:  %s ==>  %s ==> unknown\n" src dest)
              else ();
              `Send
                (_loc, (`Id (_loc, (`Lid (_loc, "self")))),
                  (`Lid (_loc, (f dest))))))
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