open StdFan
open FAstN

{:fans|keep off;
 derive (Print OPrint Map Fold MapWrapper PrintWrapper);
|};;

{:ocaml| {:include| "src/fAstN.ml" |} |};;


let wildcarder = object (self)
  inherit map as super
  method! pat = function
    | {:pat-| $lid:_ |} -> {:pat-| _ |}
    | {:pat-| ($p as $_) |} -> self#pat p
    | p -> super#pat p 
end


    

