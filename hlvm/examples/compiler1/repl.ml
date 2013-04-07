(* This is a Read-Evaluate-Print-Loop (REPL) for a tiny programming language
   to illustrate the use of HLVM.

   HLVM and this example compiler are described in detail in the OCaml Journal.
*)

open Printf
open Lexing
open Expr
open Hlvm

let rec ty : ty -> Hlvm.Type.t = function
  | `Unit -> `Unit
  | `Bool -> `Bool
  | `Int -> `Int
  | `Float -> `Float
  | `Tuple tys -> `Struct(List.map ty tys)

let rec destructure arg rest = function
  | PVar s -> Expr.Let(s, arg, rest)
  | PTup ps -> destructures arg rest 0 ps
and destructures arg rest i = function
  | [] -> rest
  | p::ps ->
      let rest = destructures arg rest (i+1) ps in
      destructure (Expr.GetValue(arg, i)) rest p

let rec compile = function
  | Unit -> Expr.Unit
  | Int n -> Expr.Int n
  | Float x -> Expr.Float x
  | Var s -> Expr.Var s
  | Apply(f, x) -> Expr.Apply(compile f, [compile x])
  | Tuple xs -> Expr.Struct(List.map compile xs)
  | UnArith(`Neg, x) -> Expr.UnArith(`Neg, compile x)
  | BinArith(op, x, y) ->
      let op = (op :> [`Add | `Div | `Mod | `Mul | `Sub]) in
      Expr.BinArith(op, compile x, compile y)
  | Cmp(op, x, y) -> Expr.Cmp(op, compile x, compile y)
  | If(p, t, f) -> Expr.If(compile p, compile t, compile f)
  | LetIn(patt, body, rest) ->
      let dummy = "frontend`arg" in
      Expr.Let(dummy, compile body,
	       destructure (Expr.Var dummy) (compile rest) patt)

let lexbuf = Lexing.from_channel stdin

let rec repl() =
  printf "# %!";
  let cont =
    try
      let f = Parse.toplevel Lex.token lexbuf in
      (*
      let ch = open_out "expr.dat" in
      output_value ch f;
      close_out ch;
      *)
      match f with
      | None ->
	  printf "\n";
	  false
      | Some(Expr f) ->
	  Hlvm.eval(`Expr(compile f));
	  true
      | Some(Defun(f, p, ty_x, ty_ret, body)) ->
	  let dummy = "frontend`arg" in
	  Hlvm.eval(`Function(f, [dummy, ty ty_x], ty ty_ret,
			      destructure (Expr.Var dummy) (compile body) p));
	  true
    with exn ->
      printf "Error: %s at line %d\n%!"
	(Printexc.to_string exn) lexbuf.lex_curr_p.pos_lnum;
      true in
  if cont then repl()

let () =
  List.iter Hlvm.eval
    [`Extern("putchar", [`Int], `Unit);
     `Function("print_char", ["c", `Int], `Unit,
	       Expr.Apply(Expr.Var "putchar", [Expr.Var "c"]));
     `Function("float_of_int", ["n", `Int], `Float,
	       Expr.FloatOfInt(Expr.Var "n"));
     `Function("int_of_float", ["x", `Float], `Int,
	       Expr.IntOfFloat(Expr.Var "x"))
    ];
  repl()
