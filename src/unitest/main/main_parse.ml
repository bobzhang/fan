open OUnit







let suite = 
  "main_parse">:::
  [
   Location_ident.suite;
   Quotation_expand.suite;
   Grammar_test.suite;
 ]
;;

let _ = 
  run_test_tt_main suite
;;  

(* local variables: *)
(* compile-command: "cd .. && pmake test_hot" *)
(* end: *)
