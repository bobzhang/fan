
open StdLib
open Ast
let strip_loc_ant ant = ant ;;
{:fans|keep off;
 derive((* Map2 Fold2 OIter   OEq *)
   Print OPrint Map Fold Strip  MapWrapper PrintWrapper);
|};;


{:ocaml|{:include| "src/Ast.mli"|}  |};;

(* change all the [loc] to [ghost] *)    
class reloc _loc = object
  inherit map 
  method! loc _ = _loc
end

(*
  {[]}
 *)  
let wildcarder = object (self)
  inherit map as super
  method! pat = function
    | {:pat| $lid:_ |} -> {:pat| _ |}
    | {:pat| ($p as $_) |} -> self#pat p
    | p -> super#pat p 
end


    
