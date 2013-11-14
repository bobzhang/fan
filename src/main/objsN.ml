open StdFan
open FAstN

%fans{keep off;
      derive (Print OPrint Map Fold MapWrapper PrintWrapper);};;

%ocaml{ %include{ "fAstN.ml" };; };;


let wildcarder = object (self)
  inherit map as super
  method! pat = function
    | %pat-{ $lid:_ } -> %pat-{ _ }
    | %pat-{ ($p as $_) } -> self#pat p
    | p -> super#pat p 
end


    


(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/objsN.cmo" *)
(* end: *)
