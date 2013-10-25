type lident = [ `Lid of Locf.t * string ]
and t =
    [ `Alias of Locf.t * t * lident
    | `Ant of Locf.t * Tokenf.ant
    | `Any of Locf.t
    | `App of Locf.t * t * t
    (* | `Bar of Locf.t * t * t *)
    | `Com of Locf.t * t * t
    | `Lid of Locf.t * string
    | `Str of Locf.t * string
    | `Vrn of Locf.t * string ]

class map :
  object
    method ant : FAst.ant -> FAst.ant
    method lident : lident -> lident
    method loc : Location.t -> Location.t
    method t : t -> t
    method string : string -> string
  end

val wildcarder : map

(* val simple : t Gramf.t *)

val to_string : t -> string
