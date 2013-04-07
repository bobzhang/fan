(** Factorial function. *)
let rec factorial(n: float) : float =
  if n < 2.0 then n else n * factorial(n - 1.0);;

factorial 10.0;;

let rec swap((x, y): int * float) : float * int = y, x;;

swap(3, 4.5);;

(** ASCII Mandelbrot renderer. *)
let rec zadd(((r1, i1), (r2, i2)) : (float * float) * (float * float)) : float * float =
  r1 + r2, i1 + i2;;

zadd((1.2, 2.3), (3.4, 4.5));;

let rec zsqr((r, i) : float * float) : float * float =
  r * r - i * i, 2.0 * r * i;;

zsqr(1.2, 2.3);;

let rec pixel((n, zr, zi, cr, ci) : int * float * float * float * float) : unit =
  if n = 65536 then print_char ' ' else
    if zr * zr + zi * zi >= 4.0 then print_char 'X' else
      pixel(n+1, zr * zr - zi * zi + cr, 2.0 * zr * zi + ci, cr, ci);;

let rec row((i, j, n) : int * int * int) : unit =
  if i <= n then
    begin
      let cr = 2.0 * float_of_int i / float_of_int n - 1.5 in
      let ci = 2.0 * float_of_int j / float_of_int n - 1.0 in
      pixel(0, 0.0, 0.0, cr, ci);
      row(i+1, j, n)
    end;;

let rec col((j, n) : int * int) : unit =
  if j <= n then
    begin
      row(0, j, n);
      print_char '\n';
      col(j+1, n)
    end;;

let rec mandelbrot(n : int) : unit =
  col(0, n);;

mandelbrot 77;;


(** n-queens solver. *)
type list = Nil of unit | Cons of (int * int) * list;;

let rec len(xs: list) : int =
  match xs with
    Nil() -> 0
    | Cons(x, xs) -> 1 + len xs;;

let rec safe ((x1, y1), (x2, y2) : (int * int) * (int * int)) : bool =
  x1 <> x2 && y1 <> y2 && x2 - x1 <> y2 - y1 && x1 - y2 <> x2 - y1;;

let rec filter ((q, xs): (int * int) * list) : list =
  match xs with
    Nil() -> Nil()
    | Cons(p, xs) ->
	let xs = filter(q, xs) in
      if safe(p, q) then Cons(p, xs) else xs;;

let rec search ((n, qs, ps): int * list * list) : int =
  match ps with
    Nil() -> if len qs = n then 1 else 0
    | Cons(q, ps) ->
      search(n, Cons(q, qs), filter(q, ps)) + search(n, qs, ps);;

let rec init((n, (x, y)): int * (int * int)) : list =
  if y=0 then Nil() else
    if x=0 then init(n, (n, y-1)) else
      Cons((x, y), init(n, (x-1, y)));;

search(8, Nil(), init(8, (8, 8)));;



(* Symbolic derivative *)
type expr =
    Int of int
  | Var of int
  | Add of expr * expr
  | Mul of expr * expr;;

let rec cmp_int((m, n): int * int) : int =
  if m<n then -1 else
    if m=n then 0 else 1;;

let rec cmp((f, g): expr * expr) : int =
  match f, g with
    Int m, Int n -> cmp_int(m, n)
    | Int _, Var _ -> -1
    | Int _, Add _ -> -1
    | Int _, Mul _ -> -1
    | Var x, Var y -> cmp_int(x, y)
    | Var _, Add _ -> -1
    | Var _, Mul _ -> -1
    | Add(f0, g0), Add(f1, g1) ->
	let c = cmp(f0, f1) in
      if c<>0 then c else
        cmp(g0, g1)
    | Add _, Mul _ -> -1
    | Mul(f0, g0), Mul(f1, g1) ->
	let c = cmp(f0, f1) in
      if c<>0 then c else
        cmp(g0, g1)
    | _ -> 1;;

let rec add((f, g): expr * expr) : expr =
  match f, g with
    Int m, Int n -> Int(m + n)
    | Int m, Add(Int n, f) -> Add(Int(m + n), f)
    | f, Int 0 -> f
    | Int 0, f -> f
    | Add(f, g), h -> add(f, add(g, h))
    | f, Add(g, h) ->
      (match cmp(f, g) with
        -1 -> Add(f, Add(g, h))
	 | 0 -> Add(Mul(Int 2, f), h)
	 | _ -> add(g, add(f, h)))
    | f, g ->
      match cmp(f, g) with
        -1 -> Add(f, g)
	| 0 -> Mul(Int 2, f)
	| _ -> Add(g, f);;

let rec mul((f, g): expr * expr) : expr =
  match f, g with
    Int m, Int n -> Int(m * n)
    | Int m, Mul(Int n, f) -> Mul(Int(m * n), f)
    | f, Int 0 -> Int 0
    | Int 0, f -> Int 0
    | f, Int 1 -> f
    | Int 1, f -> f
    | Mul(f, g), h -> mul(f, mul(g, h))
    | f, Mul(g, h) ->
      (match cmp(f, g) with
        1 -> mul(g, mul(f, h))
	 | _ -> Mul(f, Mul(g, h)))
    | f, g ->
      match cmp(f, g) with
        1 -> mul(g, f)
	| _ -> Mul(f, g);;

let rec d((f, x) : expr * int) : expr =
  match f with
    Int n -> Int 0
    | Var y -> Int(if x=y then 1 else 0)
    | Add(f, g) -> add(d(f, x), d(g, x))
    | Mul(f, g) -> add(mul(f, d(g, x)), mul(g, d(f, x)));;

let x = Var 0 in
let f = add(add(mul(mul(x, x), x), x), Int(-1)) in
f,
d(f, 0),
d(d(f, 0), 0),
d(d(d(f, 0), 0), 0);;
