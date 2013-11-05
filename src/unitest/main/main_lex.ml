

open OUnit

let suite =
  "main_lex" >:::
  [
   Lexing_test.suite;
  ]

let _ =
  run_test_tt_main suite
;;    
