open OUnit







let suite = 
"suite">:::
 [
  "Lexing_test.test_empty_string" >:: Lexing_test.test_empty_string;

  "Lexing_test.test_escaped_string" >:: Lexing_test.test_escaped_string;

  "Lexing_test.test_comment_string" >:: Lexing_test.test_comment_string;

  "Lexing_test.test_quotation" >:: Lexing_test.test_quotation;

  "Lexing_test.test_quotation" >:: Lexing_test.test_string;
  
  "Lexing_test.test_char" >:: Lexing_test.test_char;

  "Location_ident.test_ident_location" >:: Location_ident.test_ident_location
 ]
;;

let _ = 
  run_test_tt_main suite
;;  

(* local variables: *)
(* compile-command: "cd .. && make test" *)
(* end: *)
