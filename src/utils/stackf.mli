
type 'a t = { mutable elts : 'a list; mutable length : int; }

exception Empty

val invariant : 'a t -> unit

val create : unit -> 'a t

val set : 'a t -> 'a list -> int -> unit

val push : 'a -> 'a t -> unit

val pop_exn : 'a t -> 'a

val pop : 'a t -> 'a option

val top_exn : 'a t -> 'a

val top : 'a t -> 'a option

val clear : 'a t -> unit

val copy : 'a t -> 'a t

val length : 'a t -> int

val is_empty : 'a t -> bool

val iter : 'a t -> f:('a -> unit) -> unit

val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b

val exists : 'a t -> f:('a -> bool) -> bool

val for_all : 'a t -> f:('a -> bool) -> bool

val find_map : 'a t -> f:('a -> 'b option) -> 'b option

val to_list : 'a t -> 'a list

val of_list : 'a list -> 'a t

val to_array : 'a t -> 'a array

val until_empty : 'a t -> ('a -> 'b) -> unit

val topn_rev : int -> 'a t -> 'a list    

    


