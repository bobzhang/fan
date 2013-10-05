type 'a t

val create : ?random:bool -> int -> 'a t

val add : 'a t-> 'a -> unit

val remove : 'a t -> 'a -> unit

val mem : 'a t  -> 'a -> bool

val iter : ('a -> unit) -> 'a t -> unit

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val elements : 'a t-> int

val clear : 'a t -> unit

val of_list : ?size:int -> 'a list -> 'a t
    
val add_list : 'a t  -> 'a list -> unit

val to_list : 'a t -> 'a list

