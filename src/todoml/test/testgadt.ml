open Format

type _ term =
  | Int : int -> int term
  | App : ('b -> 'a) term * 'b term -> 'a term



















