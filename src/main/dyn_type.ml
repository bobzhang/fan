type t =
  | Unit | Bool | Float | Char | String
  | Int of int option
  | List of t
  | Array of t
  | Tuple of t list
  | Dict of [`R|`O] * (string * [`RW|`RO] * t) list
  | Sum of [`P|`N] * (string * t list) list
  | Option of t
  | Rec of string * t
  | Var of string
  | Arrow of t * t
  | Ext of string * t

type t =
  | Unit
  | Int of int64
  | Bool of bool
  | Float of float
  | String of string
  | Enum of t list (* all the t of the list should be of same type *)
  | Tuple of t list
  | Dict of (string * t) list
  | Sum of string * t list
  | Null
  | Value of t
  | Arrow of string
  | Rec of (string * int64) * t
  | Var of (string * int64)
  | Ext of (string * int64) * t
