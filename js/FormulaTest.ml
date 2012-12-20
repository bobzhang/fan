open OUnit
open Formula

let formula_of_string s = Parser.parse_formula Lexer.lex (Lexing.from_string s)

let tests = "Formula" >::: [
  "nnf" >::
    (fun () ->
      assert_equal ~printer:(fun x -> str x)
        (formula_of_string "((not X) or Y)") 
        (nnf (formula_of_string ("not (X and not Y)")))
    );
                        ]
