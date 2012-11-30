
(* open FanUtil; *)
(* let n = "h" and s = "c"; *)
(* let _loc = FanLoc.ghost in *)
(* with "expr" {| $(tup: {| $(anti:mk_anti ~c:"expr" n s) |}) |}; *)



let u = with "expr" fun
  [{@loc| fun [ $(pat:veryverylong)  -> y]|} -> 2 ];
  
(* with "expr" {|  $(anti:mk_anti ~c:"expr" n      s)|}; *)
(* $(agh:      ghsogh) *)
(* $(a:b) *)
(* $a:b *)
(* $(aghosghoshgohohoghos:c) *)
(* $`g:b *)

(* a $u   *)
(* {@loc| fun [ $(veryverylong)  -> y]|} *)
(* {| fun [ $(veryverylong)  -> y]|}      *)
