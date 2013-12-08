
val may : ('a -> unit) -> 'a option -> unit
val map : ('a -> 'b) -> 'a option -> 'b option

val bind :  'a option -> ('a -> 'b option) -> 'b option

val apply : ('a -> 'a) option -> 'a -> 'a
val filter : ('a -> bool) -> 'a option -> 'a option
val default : 'a -> 'a option -> 'a
val is_some : 'a option -> bool
val is_none : 'a option -> bool
val get_exn : 'a option -> exn -> 'a
val get : 'a option -> 'a
val map_default : ('a -> 'b) -> 'b -> 'a option -> 'b
val compare : ?cmp:('a -> 'a -> int) -> 'a option -> 'a option -> int
val eq : ?eq:('a -> 'a -> bool) -> 'a option -> 'a option -> bool

