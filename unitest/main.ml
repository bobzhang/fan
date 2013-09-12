open OUnit



let naive _ = assert_equal 3 3



let suite = 
"suite">:::
 ["test1">:: naive ;
  "Lexing_test.test_empty_string" >:: Lexing_test.test_empty_string
 ]
;;

let _ = 
  run_test_tt_main suite
;;  
