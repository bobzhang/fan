open Format;

#directory "_build/dyplib/";
#load "dypgen_parser.cma";
#load "dypgen_lexer.cma";
#load "dypgen_backend_string.cma";
#load "fan_common.cmo";
#load "dypgen_backend_string.cma";
value print_position (_fmt:formatter) (_pos:Lexing.position) = ();
#install_printer print_position;
open Parse_tree;  
open Lib_common;
(* Toploop.max_printer_depth.val := 100; *)
(* Toploop.max_printer_steps.val := 100; *)
#print_depth 1000;
#print_length 1000;  
value from_str str =
  Lexing.from_string str
  |>  Dypgen_parser.main (Dypgen_lexer.create_token ());

value test_str = "
%start main
%relation pi < pt < pp
%layout [' ' '\t']
%parser
main: expr \"\n\" {$1}
expr:
  | ['0'-'9']+      { int_of_string $1 } pi
  | \"-\" expr(=pi)            { -$2 }     pi
  | \"(\" expr \")\"             { $2 }      pi
  | expr(<=pp) \"+\" expr(<pp) { $1 + $3 } pp
  | expr(<=pp) \"-\" expr(<pp) { $1 - $3 } pp
  | expr(<=pt) \"*\" expr(<pt) { $1 * $3 } pt
  | expr(<=pt) \"/\" expr(<pt) { $1 / $3 } pt
";

value  (p,l,g) = test_str
  |> Lexing.from_string
  |> Dypgen_parser.main
      (Dypgen_lexer.create_token ())
  |> (fun [
      [((_,_,parser_param_info,lexer,grammar,_,_,_,_),_)
           ] -> (parser_param_info, lexer,grammar)]);

open Dypgen_backend_string;
module R = StringOutput (struct value parser_param_info = p;
  value lexer = l; value grammar = g ; end);

#install_printer print_string;
