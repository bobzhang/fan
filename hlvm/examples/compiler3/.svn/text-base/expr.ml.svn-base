type ty =
    [ `Unit
    | `Bool
    | `Byte
    | `Int
    | `Int64
    | `Float
    | `Tuple of ty list
    | `Array of ty
    | `Union of (string * ty) list
    | `Name of string ]

type binarith = [`Add|`Sub|`Mul|`Div|`Mod]

type cmp = [`Lt|`Le|`Eq|`Ne|`Ge|`Gt]

type patt =
  | PConstr of string * patt
  | PVar of string
  | PTup of patt list
  | PLit of t

and t =
  | Unit
  | Bool of bool
  | Byte of int
  | Int of int
  | Int64 of Int64.t
  | Float of float
  | String of string
  | Var of string
  | Apply of t * t
  | Tuple of t list
  | UnArith of [`Neg] * t
  | BinArith of binarith * t * t
  | Cmp of cmp * t * t
  | If of t * t * t
  | LetIn of patt * t * t
  | ArrayGet of t * t
  | ArraySet of t * t * t
  | Match of t * (patt * t) list

let rewrite r = function
  | Unit | Bool _ | Byte _ | Int _ | Int64 _ | Float _ | String _
  | Var _ as f -> f
  | Apply(f, x) -> Apply(r f, r x)
  | Tuple xs -> Tuple(List.map r xs)
  | UnArith(op, x) -> UnArith(op, r x)
  | BinArith(op, x, y) -> BinArith(op, r x, r y)
  | Cmp(op, x, y) -> Cmp(op, r x, r y)
  | If(p, t, f) -> If(r p, r t, r f)
  | LetIn(p, f, g) -> LetIn(p, r f, r g)
  | ArrayGet(a, i) -> ArrayGet(r a, r i)
  | ArraySet(a, i, x) -> ArraySet(r a, r i, r x)
  | Match(f, rs) -> Match(r f, List.map (fun (p, f) -> p, r f) rs)

type toplevel =
  | Expr of t
  | DefUnion of string * (string * ty) list
  | Defun of string * patt * ty * ty * t
