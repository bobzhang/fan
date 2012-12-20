open Formula
  
let formula_of_string s = Parser.parse_formula Lexer.lex (Lexing.from_string s)

let main =
  let run_tests = ref false in
  let opts = [
    ("-t", Arg.Unit (fun () -> run_tests := true), "run unit tests");
  ] in
  Arg.parse opts (fun _ -> ()) "Try -help for help or one of the following.";
  if !run_tests then 
    ignore (OUnit.run_test_tt ~verbose:true FormulaTest.tests)
  else
    print_endline (str (nnf (formula_of_string (read_line ()))))
