

include module type of Queue with type 'a t = 'a Queue.t

val find : 'a t -> f:('a -> bool) -> 'a option

val find_map : 'a t -> f:('a -> 'b option) -> 'b option

val to_list_rev : 'a t -> 'a list

val of_list : 'a list -> 'a t

val rev : 'a t -> 'a t
