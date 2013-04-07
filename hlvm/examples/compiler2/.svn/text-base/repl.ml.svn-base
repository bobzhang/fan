open Printf
open Lexing
open Expr
open Hlvm

type type_def =
    { args: int; llty_of: Hlvm.Type.t list -> Hlvm.Type.t }

let type_defs =
  ref ["array",
       { args = 1;
	 llty_of = function [ty] -> `Array ty | _ -> invalid_arg "array_ty" }]

let rec ty : ty -> Hlvm.Type.t = function
  | `Unit -> `Unit
  | `Bool -> `Bool
  | `Int -> `Int
  | `Float -> `Float
  | `Tuple tys -> `Struct(List.map ty tys)
  | `Array t -> `Array(ty t)

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
  | Var "false" -> Expr.Bool false
  | Var "true" -> Expr.Bool true
  | Var "pi" -> Expr.Float(4. *. atan 1.)
  | Var(s) -> Expr.Var s
  | Apply(Var "print", String s) -> Expr.Printf(s, [])
  | Apply(Var "print", a) -> Expr.Print(compile a)
  | Apply(Var "length", a) -> Expr.Length(compile a)
  | Apply(Var "create", Tuple[n; x]) -> Expr.Alloc(compile n, compile x)
  | Apply(f, x) -> Expr.Apply(compile f, [compile x])
  | Tuple xs -> Expr.Struct(List.map compile xs)
  | UnArith(`Neg, x) -> Expr.UnArith(`Neg, compile x)
  | BinArith(op, x, y) ->
      let op = (op :> [`Add | `Div | `Mod | `Mul | `Sub]) in
      Expr.BinArith(op, compile x, compile y)
  | Cmp(op, x, y) -> Expr.Cmp(op, compile x, compile y)
  | If(p, t, f) -> Expr.If(compile p, compile t, compile f)
  | LetIn(p, f, g) ->
      let dummy = "frontend`letindummy" in
      Expr.Let(dummy, compile f, destructure (Expr.Var dummy) (compile g) p)
  | ArrayGet(a, i) -> Expr.Get(compile a, compile i)
  | ArraySet(a, i, x) -> Expr.Set(compile a, compile i, compile x)
  | String _ -> invalid_arg "Strings may only appear in print<string>"

let lexbuf = Lexing.from_channel stdin

open Parse

let token lexbuf =
  let tok = Lex.token lexbuf in
  let s =
    match tok with
    | LET -> "LET"
    | REC -> "REC"
    | IN -> "IN"
    | IF -> "IF"
    | THEN -> "THEN"
    | ELSE -> "ELSE"
    | PIPE -> "PIPE"
    | COMMA -> "COMMA"
    | OPEN -> "OPEN"
    | CLOSE -> "CLOSE"
    | LT -> "LT"
    | LE -> "LE"
    | EQ -> "EQ"
    | NE -> "NE"
    | GE -> "GE"
    | GT -> "GT"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | TIMES -> "TIMES"
    | DIVIDE -> "DIVIDE"
    | CONS -> "CONS"
    | INT n -> "INT "^n
    | FLOAT x -> "FLOAT "^x
    | IDENT s -> "IDENT \""^s^"\""
    | SEMI -> "SEMI"
    | SEMISEMI -> "SEMISEMI"
    | _ -> "<tok>" in
  printf "Token: %s\n%!" s;
  tok

let rec repl() =
  printf "# %!";
  begin
    try
      let f = Parse.toplevel Lex.token lexbuf in
      let ch = open_out "expr.dat" in
      output_value ch f;
      close_out ch;
      match f with
      | Expr f -> Hlvm.eval(`Expr(compile f))
      | Defun(f, p, ty_x, ty_ret, body) ->
	  let dummy = "frontend`defundummy" in
	  Hlvm.eval(`Function(f, [dummy, ty ty_x], ty ty_ret,
			      destructure (Expr.Var dummy) (compile body) p))
    with
    | End_of_file -> raise End_of_file
    | exn ->
	printf "Error: %s at line %d, char %d\n%!"
	  (Printexc.to_string exn)
	  lexbuf.lex_curr_p.pos_lnum
	  lexbuf.lex_curr_p.pos_cnum
  end;
  repl()

let () =
  List.iter Hlvm.eval
    [`Extern("putchar", [`Int], `Unit);
     `Extern("sin", [`Float], `Float);
     `Extern("cos", [`Float], `Float);
     `Function("print_char", ["c", `Int], `Unit,
	       Expr.Apply(Expr.Var "putchar", [Expr.Var "c"]));
     `Function("float_of_int", ["n", `Int], `Float,
	       Expr.FloatOfInt(`Float, Expr.Var "n"));
     `Function("int_of_float", ["x", `Float], `Int,
	       Expr.IntOfFloat(`Int, Expr.Var "x"));
     `Function("float_array", ["nx", `Struct[`Int; `Float]], `Array `Float,
	       Expr.Alloc(Expr.GetValue(Expr.Var "nx", 0),
			  Expr.GetValue(Expr.Var "nx", 1)))
    ];
(*
  Hlvm.debug := true;
*)
  try
    repl()
  with End_of_file -> Hlvm.save()
