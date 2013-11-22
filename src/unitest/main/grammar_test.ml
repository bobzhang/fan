


open OUnit
(* open Test_util *)

(* not stable -- so comment here*)  
(* let () = *)
(*   Ast_parsers.use_parsers ["revise"] *)

(* let t_meta  = Gramlib.parse_string Parse_parse.simple *)

(* let test_simple_meta _ = *)
(*   if *)
(*     "Lid" *)
(*     |> t_meta *)
(*     |> %p{[{ txt  =   (\* magic number left, to be more precise later *\) *)
(*              [{text = Token (_, %exp@_{( $_, `Any)}) ; _}];_}]} *)
(*     |> not then *)
(*     assert_failure "test_simple_meta" *)

(* let test_simple_meta1 _ = *)
(*   if *)
(*     "Lid x" *)
(*     |> t_meta *)
(*     |> %p{[{txt = *)
(*             [{text = Token (_, %exp@_{ ($_,`Any) }) *)
(*               ; _ }];_}]} *)
(*     |> not then *)
(*     %err{test_simple_meta1} *)

  
(* let test_simple_meta2 _ = *)
(*   if *)
(*     %str{("ghso"|"a" as x)} *)
(*     |> t_meta *)
(*     |> %p{[{txt = *)
(*             [ *)
(*             {text =  `Keyword(_,"ghso"); *)
(*              styp = %ctyp'@_{Tokenf.txt} ; (\* Wrong?*\) *)
(*              ;_}];_}; *)
(*            {txt =  *)
(*             [ *)
(*              {text =  `Keyword(_,"a"); *)
(*              styp =  %ctyp'@_{Tokenf.txt} ; (\* Wrong?*\) *)
(*               ;_}];_}]} *)
(*     |> not then *)
(*     %err{test_simple_meta2} *)



let suite =
  "Grammar_test" >::: []
  (* ["test_simple_meta" >:: test_simple_meta; *)
  (*  "test_simple_meta1" >:: test_simple_meta1; *)
  (*  "test_simple_meta2" >:: test_simple_meta2 ] *)
    

(* local variables: *)
(* compile-command: "cd .. && pmake unitest_annot/grammar_test.cmo" *)
(* end: *)
