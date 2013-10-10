open OUnit







let suite = 
  "main_parse">:::
  [
   Location_ident.suite;
 ]
;;

let _ = 
  run_test_tt_main suite
;;  

(* local variables: *)
(* compile-command: "cd .. && make test" *)
(* end: *)
