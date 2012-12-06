
open FanUtil;
let n = "h" and s = "c";
let _loc = FanLoc.ghost ;

with "expr" {:expr| $(anti:mk_anti ~c:"expr" n s) |};
with "expr" {:expr| $(tup: {| $(anti:mk_anti ~c:"expr" n s) |}) |};
let u = with "expr" fun
  [{@loc| fun [ $(pat:veryverylong)  -> y]|} -> 2 ];
  
with "expr" {|  $(anti:mk_anti ~c:"expr" n      s)|};
(* $(agh:      ghsogh) *)
(* $(a:b) *)
(* $a:b *)
(* $(aghosghoshgohohoghos:c) *)
(* $`g:b *)

(* a $u   *)
(* {@loc| fun [ $(veryverylong)  -> y]|} *)
(* {| fun [ $(veryverylong)  -> y]|}      *)
(* {:ctyp| `$i |} *)

(* $i *)
(* $(a:b) *)
(* `$i   *)
(* {:expr| $(anti:mk_anti ~c:"expr" n s) |} *)
(* {| $(anti:mk_anti ~c:"expr" n s) |} *)
(* a   *)
with "sig_item" fun
  [ {@loc|   # $x $_ |} -> x ];

with "expr" fun
  [ {| $e1.$exp:e2|} -> ()
  | {:expr@loc| A $uid:s |} as e -> ()
  | {| $(id:({:ident| $_.$_ |} as i)) |} -> ()
  | {| A $flo:x |} -> () ];

  
let u x = {:patt| $flo:x |};
let f x = {:expr| $tup:x |};
  
let u x y = {:expr| $lid:x $y |}; 
