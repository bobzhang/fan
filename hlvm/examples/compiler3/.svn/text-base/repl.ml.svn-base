open Printf
open Lexing
open Expr
open Hlvm

type ty_env = { defs: (string * ty) list }

(** Convert an ML type into an HLVM type. *)
let rec ty ty_env : ty -> Hlvm.Type.t = function
  | `Unit -> `Unit
  | `Bool -> `Bool
  | `Byte -> `Byte
  | `Int -> `Int
  | `Int64 -> `Int64
  | `Float -> `Float
  | `Tuple tys -> `Struct(List.map (ty ty_env) tys)
  | `Array t -> `Array(ty ty_env t)
  | `Union _ -> `Reference
  | `Name ty_name -> ty ty_env (List.assoc ty_name ty_env.defs)

let unique =
  let n = ref 0 in
    fun () -> incr n; !n

(** Convert an identifier into its unique name. *)
let name vars x = sprintf "%s$%d" x (vars x)

(** Create a new mapping from identifiers to their unique names that includes
    a new binding for 'x'. *)
let next vars x y = vars y + if x=y then 1 else 0

(** Helper function to apply a function to the second element of a tuple. *)
let apply f (vars, x) = vars, f x

(** Map over a list accumulating a result. *)
let rec mapfold f a = function
  | [] -> a, []
  | x::xs ->
      let a, x = f a x in
      let a, xs = mapfold f a xs in
	a, x::xs

(** Substitute unique identifier names in patterns. *)
let rec patt vars = function
  | PConstr(c, p) -> apply (fun p -> PConstr(name vars c, p)) (patt vars p)
  | PVar x ->
      let vars = next vars x in
	vars, PVar(name vars x)
  | PTup ps -> apply (fun ps -> PTup ps) (mapfold patt vars ps)
  | PLit _ as p -> vars, p

(** Substitute unique identifier names in expressions. *)
let rec alpha vars = function
  | Var x -> Var(name vars x)
  | LetIn(p, f, g) ->
      let vars', p = patt vars p in
	LetIn(p, alpha vars f, alpha vars' g)
  | Match(f, rs) ->
      let rec case vars (p, f) =
	let vars', p = patt vars p in
	  p, alpha vars' f in
	Match(alpha vars f, List.map (case vars) rs)
  | f -> rewrite (alpha vars) f

(** Try to match an argument to a pattern and execute the "rest"
    expression with its pattern variables bound, or execute "fail"
    if it does not match. *)
let rec bind arg patt rest fail = match fail, patt with
  | None, PConstr(constr, patt) ->
      Expr.compound
	[ Expr.If(Expr.IsType(arg, constr), Expr.Unit,
		  Expr.compound
		    [ Expr.Printf("Match failure on '", []);
		      Expr.Print(arg);
		      Expr.Printf("'\n", []);
		      Expr.Exit(Expr.Int 1) ]);
	  let dummy = sprintf "frontend`tmp%d" (unique()) in
	  Expr.Let(dummy, Expr.Cast(arg, constr),
		   bind (Expr.Var dummy) patt rest fail) ]
  | Some fail, PConstr(constr, patt) ->
      (* FIXME: The "fail" branch is compiled twice leading to exponential
	 growth of generated code as a function of the number of match
	 cases. *)
      Expr.If(Expr.IsType(arg, constr),
	      (let dummy = sprintf "frontend`tmp%d" (unique()) in
	       Expr.Let(dummy, Expr.Cast(arg, constr),
			bind (Expr.Var dummy) patt rest (Some fail))),
	      fail)
  | _, PVar var -> Expr.Let(var, arg, rest)
  | fail, PTup patts -> binds 0 arg patts rest fail
  | _, PLit Unit -> rest
  | Some fail, PLit(Bool b) -> Expr.If(Expr.Cmp(`Eq, arg, Expr.Bool b), rest, fail)
  | Some fail, PLit(Int n) -> Expr.If(Expr.Cmp(`Eq, arg, Expr.Int n), rest, fail)
  | Some fail, PLit(Float x) -> Expr.If(Expr.Cmp(`Eq, arg, Expr.Float x), rest, fail)
  | None, PLit(Int _ | Float _) -> invalid_arg "bind of (Bool|Int|Float) without fail case"
  | _, PLit _ -> invalid_arg "bind of non-(Unit|Bool|Int|Float) literal pattern"

(** Try to match all of the elements of a tuple. *)
and binds i args patts rest fail = match patts with
  | [] -> rest
  | patt::patts ->
      let rest = binds (i+1) args patts rest fail in
      bind (Expr.GetValue(args, i)) patt rest fail

(** Match a value against a sequence of match cases. *)
let rec pmatch arg = function
  | [] -> invalid_arg "pmatch []"
  | [patt, expr] -> bind arg patt expr None
  | (patt, expr)::rules -> bind arg patt expr (Some(pmatch arg rules))

(** Compile an ML expression into an HLVM expression. *)
let rec compile = function
  | Unit -> Expr.Unit
  | Bool b -> Expr.Bool b
  | Byte c -> Expr.Byte c
  | Int n -> Expr.Int n
  | Int64 n -> Expr.Int64 n
  | Float x -> Expr.Float x
  | Var "false$1" -> Expr.Bool false
  | Var "true$1" -> Expr.Bool true
  | Var "pi$1" -> Expr.Float(4. *. atan 1.)
  | Var s -> Expr.Var s
  | Apply(Var "print$1", String s) -> Expr.Printf(s, [])
  | Apply(Var "print$1", a) -> Expr.Print(compile a)
  | Apply(Var "length$1", a) -> Expr.Length(compile a)
  | Apply(Var "create$1", Tuple[n; x]) -> Expr.Alloc(compile n, compile x)
  | Apply(f, x) -> Expr.Apply(compile f, [compile x])
  | Tuple xs -> Expr.Struct(List.map compile xs)
  | UnArith(`Neg, x) -> Expr.UnArith(`Neg, compile x)
  | BinArith(op, x, y) -> Expr.BinArith((op :> binarith), compile x, compile y)
  | Cmp(op, x, y) -> Expr.Cmp(op, compile x, compile y)
  | If(p, t, f) -> Expr.If(compile p, compile t, compile f)
  | LetIn(p, f, g) -> pmatch (compile f) [p, compile g]
  | ArrayGet(a, i) -> Expr.Get(compile a, compile i)
  | ArraySet(a, i, x) -> Expr.Set(compile a, compile i, compile x)
  | String _ -> invalid_arg "Not implemented: strings may only appear as the first argument to 'print'"
  | Match(f, rs) -> pmatch (compile(LetIn(PVar "arg", f, Var "arg"))) (List.map (fun (p, g) -> p, compile g) rs)

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

let eval (ty_env, vars) = function
  | Expr f ->
      let f = alpha vars f in
      Hlvm.eval(`Expr(compile f));
      ty_env, vars
  | DefUnion(t, cs) ->
      let ty_env = { ty_env with defs = (t, `Union [])::ty_env.defs } in
      let aux vars (c, t) =
	let c = name vars c in
	let vars = next vars c in
	Hlvm.eval(`Type(c, ty ty_env t));
	Hlvm.eval(`Function(c, ["x", ty ty_env t],
			    `Reference, Expr.Construct(c, Expr.Var "x")));
	vars in
      let vars = List.fold_left aux vars cs in
      { ty_env with defs = (t, `Union cs)::ty_env.defs }, vars
  | Defun(f, p, ty_x, ty_ret, body) ->
      let vars = next vars f in
      let dummy = sprintf "frontend`fnarg%d" (unique()) in
      let body = alpha vars (LetIn(p, Var dummy, body)) in
      Hlvm.eval(`Function(name vars f, [name vars dummy, ty ty_env ty_x],
			  ty ty_env ty_ret, compile body));
      ty_env, vars

(** Provide an interactive top-level that inputs expressions and function
    definitions from the user and compiles and runs them using HLVM. *)
let rec repl (ty_env, vars) =
  printf "# %!";
  let ty_env, vars =
    try
      let f = Parse.toplevel Lex.token lexbuf in
      let ch = open_out "expr.dat" in
      output_value ch f;
      close_out ch;
      eval (ty_env, vars) f
    with
    | End_of_file -> raise End_of_file
    | Failure "lexing: empty token" as exn ->
	printf "Error: %s at line %d, char %d\n%!"
	  (Printexc.to_string exn)
	  lexbuf.lex_curr_p.pos_lnum
	  lexbuf.lex_curr_p.pos_cnum;
	raise exn
    | exn ->
	printf "Error: %s at line %d, char %d\n%!"
	  (Printexc.to_string exn)
	  lexbuf.lex_curr_p.pos_lnum
	  lexbuf.lex_curr_p.pos_cnum;
	ty_env, vars in
  repl (ty_env, vars)

(** Bootstrap some built-in functions before allowing the user to define
    new functions. *)
let () =
  List.iter Hlvm.eval
    [
      `Extern("putchar", [`Int], `Unit);
      `Extern("sin", [`Float], `Float);
      `Extern("cos", [`Float], `Float);
      `Function("sin$1", ["x", `Float], `Float,
		Expr.Apply(Expr.Var "sin", [Expr.Var "x"]));
      `Function("cos$1", ["x", `Float], `Float,
		Expr.Apply(Expr.Var "cos", [Expr.Var "x"]));
      `Function("print_char$1", ["c", `Int], `Unit,
		Expr.Apply(Expr.Var "putchar", [Expr.Var "c"]));
      `Function("float_of_int$1", ["n", `Int], `Float,
		Expr.FloatOfInt(`Float, Expr.Var "n"));
      `Function("int_of_float$1", ["x", `Float], `Int,
		Expr.IntOfFloat(`Int, Expr.Var "x"));
      `Function("float_array$1", ["nx", `Struct[`Int; `Float]], `Array `Float,
		Expr.Alloc(Expr.GetValue(Expr.Var "nx", 0),
			   Expr.GetValue(Expr.Var "nx", 1)))
    ];
  (*
    Hlvm.debug := true;
  *)
  try
    repl ({ defs = [] }, fun _ -> 1)
  with End_of_file ->
    Hlvm.save();
    printf "\n"
