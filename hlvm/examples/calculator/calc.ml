open Printf
open Hlvm
open Expr
open Camlp4.PreCast

let expr = Gram.Entry.mk "expr"

EXTEND Gram
  expr:
     [ [ e1 = expr; "+"; e2 = expr -> e1 +: e2
       | e1 = expr; "-"; e2 = expr -> e1 -: e2 ]
     | [ e1 = expr; "*"; e2 = expr -> e1 *: e2
       | e1 = expr; "/"; e2 = expr -> e1 /: e2 ]
     | [ n = INT -> Float(float_of_string n)
       | x = FLOAT -> Float(float_of_string x)
       | "("; e = expr; ")" -> e ] ];
END

let rec repl() =
  printf "# %!";
  begin
    try
      let e = Gram.parse expr Loc.ghost (Stream.of_channel stdin) in
      try
	Hlvm.eval(`Expr e)
      with exn ->
	printf "Eval error: %s\n%!" (Printexc.to_string exn)
    with exn ->
      printf "Parse error: %s\n%!" (Printexc.to_string exn)
  end;
  repl()

let () = repl()
