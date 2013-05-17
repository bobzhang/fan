open StdLib
open AstN

{:fans|keep off;
 derive (Print OPrint Map Fold MapWrapper PrintWrapper);
|};;

{:ocaml| {:include| "src/AstN.ml" |} |};;


let wildcarder = object (self)
  inherit map as super
  method! pat = function
    | {:pat-| $lid:_ |} -> {:pat-| _ |}
    | {:pat-| ($p as $_) |} -> self#pat p
    | p -> super#pat p 
end


    

