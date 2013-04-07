type ty =
    [ `Unit
    | `Bool
    | `Int
    | `Float
    | `Tuple of ty list
    | `Array of ty ]

type cmp = [`Lt|`Le|`Eq|`Ne|`Ge|`Gt]

type patt =
  | PVar of string
  | PTup of patt list

type t =
  | Unit
  | Int of int
  | Float of float
  | String of string
  | Var of string
  | Apply of t * t
  | Tuple of t list
  | UnArith of [`Neg] * t
  | BinArith of [`Add|`Sub|`Mul|`Div] * t * t
  | Cmp of cmp * t * t
  | If of t * t * t
  | LetIn of patt * t * t
  | ArrayGet of t * t
  | ArraySet of t * t * t

type toplevel =
  | Expr of t
  | Defun of string * patt * ty * ty * t
