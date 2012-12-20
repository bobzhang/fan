type formula =
    Var of string
  | Not of formula
  | Or of formula * formula
  | And of formula * formula
        
val str : formula -> string

val nnf : ?negate:bool -> formula -> formula
