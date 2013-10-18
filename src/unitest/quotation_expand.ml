open OUnit
open Test_util
let test_pat1 _ = 
  %pat-'{`a(a,b,c)} ===
   `App (`App (`App (`Vrn "a",`Lid "a"), `Lid "b"), `Lid "c")

let suite =
  "Quotation_expand" >:::
  ["test_pat1" >:: test_pat1]

(* local variables: *)
(* compile-command: "cd .. && pmake unitest_annot/quotation_expand.cmo" *)
(* end: *)
