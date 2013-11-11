include module type of Hashtbl with type ('a,'b) t = ('a,'b) Hashtbl.t

val keys : ('a, 'b) t -> 'a list

val values : ('a, 'b) t -> 'b list

val find_default : default:'a -> ('b, 'a) t -> 'b -> 'a

val find_opt : ('b,'a) t -> 'b -> 'a option

val add_list : ('a, 'b) t -> ('a * 'b) list -> unit
    
val mk :
    eq:('a -> 'a -> bool) ->
      hash:('a -> int) -> (module S with type key = 'a)
    
val memoize : ('a -> 'b) -> 'a -> 'b
