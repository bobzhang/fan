open OUnit







let suite = 
  "main_parse">:::
  [
   Location_ident.suite;
   Quotation_expand.suite;
   Test_grammar.suite;
 ]
;;

let _ = 
  run_test_tt_main suite
;;  

(* local variables: *)
(* compile-command: "cd .. && pmake test_hot" *)
(* end: *)
