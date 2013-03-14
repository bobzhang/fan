(* type u = *)
(*   [= `U of string];   *)
type f1 =
  [= `App of (f1 * f1)
  | `U of string ];
type 'a u = [> `App of (int * int) | 'a ] -> 'a;
  
(* let rec list_of_app  (x:f1)  (acc: list [= `U of string]) = *)
(*   match x with *)
(*   [`App(t1,t2) -> list_of_app t1 (list_of_app t2 acc) *)
(*   |  x   -> [ x :: acc] ]; *)


(* type ant = *)
(*     [= `Ant of  FanUtil.anti_cxt]; *)

(* type f = *)
(*   [= `App of (f * f) *)
(*   | `U of g *)
(*   | ant ] *)
(* and g = *)
(*   [= `E of g *)
(*   | `U of g *)
(*   | ant ]; *)


(* type f1 = *)
(*   [= `App of (f1 * f1) *)
(*   | `U of g1 ] *)
(* and g1 = *)
(*   [= `E of g1 *)
(*   | `U of g1 ]; *)

(* let f (x:f) = *)
(*   match x with *)
(*   [#f1 as y -> y *)
(*   | _ -> assert false ] ; *)
(*   (\* (x : f1 :> f); *)
(*      an easy way to transform f -> f1 *)
(*      possible.. worth??  *)
(*    *\) *)


  
