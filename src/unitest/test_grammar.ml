


open OUnit
open Test_util
  
let () =
  Ast_parsers.use_parsers ["revise"]

let t_meta  = Gramf.parse_string Parse_grammar.simple

let test_simple_meta _ =
  if
    "Lid"
    |> t_meta
    |> %p{[{text =
            `Stoken (_,
                     %exp@_{function | `Lid _ -> true | _ -> false},
                     %exp@_{( $_, `Any)},
                    (* magic number left, to be more precise later *)
                     _);
     pattern= None ;
     _}]}
    |> not then
    assert_failure "test_simple_meta"

(* (%p{[{Gram_def.text = `Stok (_, *)
(*                    %exp@_{function | `Lid _ -> true | _ -> false}, *)
(*                    %exp@_{%pat-'{`Lid _}} ,_); _}]} u) *)        
let test_simple_meta1 _ =
  if
    "Lid x"
    |> t_meta
    |> %p{[{text =
            `Stoken (_,
                     %exp@_{function | `Lid _ -> true | _ -> false},
                     %exp@_{ ($_,`Any) } ,_); (* magic number left *)
            pattern = Some %pat@_{`Lid ({txt = x;_}:Tokenf.txt) } ;
     _ }]}
    |> not then
    assert_failure "test_simple_meta1"

  
let test_simple_meta2 _ =
  if
    %str{("ghso"|"a" as x)}
    |> t_meta
    |> %p{[{text =  `Skeyword(_,"ghos");
            styp = `Tok _ ; (* Wrong?*)
            pattern =
            Some %pat@_{`Key ({txt = x;_}:Tokenf.txt)};};
           {text =  `Skeyword(_,"ghos");
            styp = `Tok _ ; (* Wrong?*)
            pattern =
            Some %pat@_{`Key ({txt = x;_}:Tokenf.txt)};}]}
    |> not then
    assert_failure "test_simple_meta2"



let suite =
  "Test_grammar" >:::
  ["test_simple_meta" >:: test_simple_meta;
   "test_simple_meta1" >:: test_simple_meta1
 ]
    

(* local variables: *)
(* compile-command: "cd .. && pmake unitest_annot/test_grammar.cmo" *)
(* end: *)
