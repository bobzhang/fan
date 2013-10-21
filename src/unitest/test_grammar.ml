


open OUnit
open Test_util
  
let () =
  Ast_parsers.use_parsers ["revise"]

let t_meta  = Fgram.parse_string Parse_grammar.simple

let test_simple_meta _ =
  let u = t_meta "Lid _" in
  match u with
  | [{text = `Stok (_,
                   %exp@_{function | `Lid _ -> true | _ -> false},
                   %exp@_{ ("Lid", `Any)} ,
                   _);
     pattern= Some %pat@_{`Lid _};
     _}] ->  ()
  | _ -> assert_failure "test_simple not token"

(* (%p{[{Gram_def.text = `Stok (_, *)
(*                    %exp@_{function | `Lid _ -> true | _ -> false}, *)
(*                    %exp@_{%pat-'{`Lid _}} ,_); _}]} u) *)        
let test_simple_meta1 _ =
  let u = t_meta "Lid x" in
  match u with
  | [{text = `Stok (_,
                   %exp@_{function | `Lid _ -> true | _ -> false},
                   %exp@_{("Lid",`Any)} ,_);
     pattern = Some %pat@_{`Lid x } ;
     _ }] ->  ()

  | _ -> assert_failure "test_simple not token"
  
let test_simple_meta2 _ =
  let u = t_meta %str{("ghso"|"a" as x)} in
  match u with
  | [{text =  `Skeyword(_,"ghos");
     styp = `Tok _ ; (* Wrong?*)
     pattern =
     Some %pat@_{`Key x};
   };
     {text =  `Skeyword(_,"ghos");
     styp = `Tok _ ; (* Wrong?*)
     pattern =
     Some %pat@_{`Key x};
   }] -> ()
  | _ -> assert_failure "test_simple_meta2 " 
    
let suite =
  "Test_grammar" >:::
  ["test_simple_meta" >:: test_simple_meta;
   "test_simple_meta1" >:: test_simple_meta1
 ]
    

(* local variables: *)
(* compile-command: "cd .. && pmake unitest_annot/test_grammar.cmo" *)
(* end: *)
