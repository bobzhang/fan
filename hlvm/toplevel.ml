open Printf
open Hlvm
open Expr
open Camlp4.PreCast

let expr = Gram.Entry.mk "expr"
let param_list = Gram.Entry.mk "param_list"
let str_item = Gram.Entry.mk "str_item"

EXTEND Gram
  expr:
     [ "apply" LEFTA
	 [ f = expr; "(" ; args = LIST0 expr SEP "," ; ")" ->
	     Apply(f, args) ]
     | "sum" LEFTA
	 [ e1 = expr; "+"; e2 = expr -> e1 +: e2
	 | e1 = expr; "-"; e2 = expr -> e1 -: e2 ]
     | "product" LEFTA
	 [ e1 = expr; "*"; e2 = expr -> e1 *: e2
	 | e1 = expr; "%"; e2 = expr -> e1 %: e2
	 | e1 = expr; "/"; e2 = expr -> e1 /: e2 ]
     | "logic" LEFTA
	 [ e1 = expr; "&&"; e2 = expr -> e1 &&: e2
	 | e1 = expr; "||"; e2 = expr -> e1 ||: e2 ]
     | "cmp" LEFTA
	 [ e1 = expr; "<"; e2 = expr -> e1 <: e2
	 | e1 = expr; "<="; e2 = expr -> e1 <=: e2
	 | e1 = expr; "="; e2 = expr -> e1 =: e2
	 | e1 = expr; "<>"; e2 = expr -> e1 <>: e2
	 | e1 = expr; ">="; e2 = expr -> e1 >=: e2
	 | e1 = expr; ">"; e2 = expr -> e1 >: e2 ]
     | [ "if"; p = expr; "then"; t = expr; "else"; f = expr ->
	   If(p, t, f) ]
     | "simple" NONA
	 [ n = INT -> Int(int_of_string n)
	 | x = FLOAT -> Float(float_of_string x)
	 | "("; e = expr; ")" -> e 
	 | v = LIDENT -> Var v ]
     ];
  param_list:
    [ [ "(" ; args = LIST1 [ v = LIDENT -> v ] SEP "," ; ")" ->
	  List.map (fun x -> x, `Float) args ]
    | [ x = LIDENT -> [x, `Float] ]
    ];
  str_item:
    [ [ "let" ; f = LIDENT ; params = param_list;  "=" ; body = expr; ";;" ->
          (`Function(f, params, `Float, body) : Hlvm.t) ]
    | [ e = expr; ";;"  -> `Expr e ]
    ];
END

let try_input_line ch =
  try Some(input_line ch) with End_of_file -> None

let rec toploop() =
  printf "> %!";
  begin
    try
      let func_def =
	Gram.parse str_item Loc.ghost (Stream.of_channel stdin) in
      try
	Hlvm.eval func_def
      with exn ->
	printf "Eval error: %s\n%!" (Printexc.to_string exn)
    with exn ->
      printf "Parse error: %s\n%!" (Printexc.to_string exn)
  end;
  toploop()

let () =
  toploop()
