
open FanUtil;
let n = "h" and s = "c";
let _loc = FanLoc.ghost in
with "expr" {| $(tup: {| $(anti:mk_anti ~c:"expr" n s) |}) |};
(* with "expr" {|  $(anti:mk_anti ~c:"expr" n      s)|}; *)
(* $(agh:      ghsogh) *)
(* $(a:b) *)
(* $a:b *)
(* $(aghosghoshgohohoghos:c) *)
(* $`g:b *)

(* a $u   *)
