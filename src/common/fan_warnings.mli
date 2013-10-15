


type t = Locf.t -> string -> unit

val emit : Locf.position -> string -> unit

val emitf : Locf.position -> ('a, unit, string, unit) format4 -> 'a

val default : t

val current : t ref

val print : t
