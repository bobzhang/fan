


open OUnit
open Test_util
  
let () =
  Ast_parsers.use_parsers ["revise"]

let t_meta  = Gramf.parse_string Parse_parse.simple

let test_simple_meta _ =
  if
    "Lid"
    |> t_meta
    |> %p{[{symbol={text =
            `Token (_,
                     %exp@_{function | `Lid _ -> true | _ -> false},
                     %exp@_{( $_, `Any)},
                    (* magic number left, to be more precise later *)
                     _);
     pattern= None ;
     _};_}]}
    |> not then
    assert_failure "test_simple_meta"

let test_simple_meta1 _ =
  if
    "Lid x"
    |> t_meta
    |> %p{[{symbol = {text =
            `Token (_,
                     %exp@_{function | `Lid _ -> true | _ -> false},
                     %exp@_{ ($_,`Any) } ,_); (* magic number left *)
            pattern = Some %pat@_{({txt = x;_}:Tokenf.txt) } ;
     _ };_}]}
    |> not then
    %err{test_simple_meta1}

  
let test_simple_meta2 _ =
  if
    %str{("ghso"|"a" as x)}
    |> t_meta
    |> %p{[{symbol=
            {text =  `Keyword(_,"ghso");
             styp = %ctyp'@_{Tokenf.txt} ; (* Wrong?*)
             pattern =
             Some %pat@_{({txt = x;_}:Tokenf.txt)};_};_};
           {symbol =
            {text =  `Keyword(_,"a");
             styp =  %ctyp'@_{Tokenf.txt} ; (* Wrong?*)
             pattern =
             Some %pat@_{ ({txt = x;_}:Tokenf.txt)};_};_}]}
    |> not then
    %err{test_simple_meta2}



let suite =
  "Grammar_test" >:::
  ["test_simple_meta" >:: test_simple_meta;
   "test_simple_meta1" >:: test_simple_meta1;
   "test_simple_meta2" >:: test_simple_meta2 ]
    

(* local variables: *)
(* compile-command: "cd .. && pmake unitest_annot/grammar_test.cmo" *)
(* end: *)
