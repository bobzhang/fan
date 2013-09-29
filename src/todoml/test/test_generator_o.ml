
open Genbase
<:fan< lang "fan" ; >>;;
module Camlp4Types = struct
  << types_of_mli "test_types.mli" ;  >> ;
end 

open Test_types

<< gen
    ( |<- "MPrint")
    ( |<- "MEq")
    ( |<- "MMap")
    ( |<- "MMap2")
    ( |<- "MFold")
    ( |<- "MFold2")
    ( |<- "OPrint");
>>    
;;


let v = { u=3;
        v = (M_cons 5
               (M_cons 4
                  (M_cons 3
                     (M_cons 2
                        (M_cons 1 M_nil)))));
        x = (( Node
                  (M_cons  Leaf 
                     (M_cons Leaf
                        (M_cons Leaf
                           (M_cons
                              (Node
                                 M_nil (E 3)
                              )
                              M_nil))))

                 (E 3 )),3)
      }
;;
printf "@[%a@]@." pp_print_k v ;;

let obj = object
  inherit  Map.map ;
  method! int x = x + 1;
end 

let fold_obj = new Fold.fold
let obj2 = new Map2.map
let p_obj = new OPrint.iter ;;
  
printf "@[%a@]@." pp_print_k (obj#k v);;


let c =   (obj2#k (v, v ));;
printf "@[%a@]@." pp_print_k c;;

printf "@[%a@]@." p_obj#k c;;
    
printf "@[%d@]" (fold_obj#k v)#getsum ;;
(*
value f = fun  [ <:ctyp< [= `$uid:cons$ of $tys$] >> -> tys];
*)
  






























