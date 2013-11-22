open OUnit
(* open Test_util *)
let test_pat1 _ =
  if 
    %pat-'{`a(a,b,c)} =
    `App(`Vrn"a", `Par(`Com(`Lid "a",`Com(`Lid "b",`Lid "c"))))
   |> not then
    %err{test_pat1}

let suite =
  "Quotation_expand" >:::
  ["test_pat1" >:: test_pat1]

(* local variables: *)
(* compile-command: "cd .. && pmake unitest_annot/quotation_expand.cmo" *)
(* end: *)
