include module type of List 

val rev_len : 'a list -> int * 'a list

val hd : 'a list -> 'a

val tl : 'a list -> 'a list

val safe_tl : 'a list -> 'a list

val null : 'a list -> bool

val drop : int -> 'a list -> 'a list

val lastbut1 : 'a list -> 'a list * 'a

val last : 'a list -> 'a

val split_at : int -> 'a list -> 'a list * 'a list

val find_map : ('a -> 'b option) -> 'a list -> 'b option

val fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> int * 'a

val remove : 'a -> ('a * 'b) list -> ('a * 'b) list

val iteri : (int -> 'a -> unit) -> 'a list -> unit

val reduce_left : ('a -> 'a -> 'a) -> 'a list -> 'a

val reduce_left_with :
    compose:('a -> 'a -> 'a) -> project:('b -> 'a) -> 'b list -> 'a
val reduce_right_with :
    compose:('a -> 'a -> 'a) -> f:('b -> 'a) -> 'b list -> 'a
val reduce_right : ('a -> 'a -> 'a) -> 'a list -> 'a

val init : int -> (int -> 'a) -> 'a list

val concat_map : ('a -> 'b list) -> 'a list -> 'b list

val filter_map : ('a -> 'b option) -> 'a list -> 'b list

val take_rev : int -> 'a list -> 'a list

val find_opt : ('a -> bool) -> 'a list -> 'a option

val cross : 'a list list -> 'a list list 
