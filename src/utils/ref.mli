val protect : 'a ref -> 'a -> (unit -> 'b) -> 'b
val safe : 'a ref -> (unit -> 'b) -> 'b
val protect2 : 'a ref * 'a -> 'b ref * 'b -> (unit -> 'c) -> 'c
val save2 : 'a ref -> 'b ref -> (unit -> 'c) -> 'c
val protects : 'a ref list -> 'a list -> (unit -> 'b) -> 'b
val saves : 'a ref list -> (unit -> 'b) -> 'b
val post : 'a ref -> ('a -> 'a) -> 'a
val pre : 'a ref -> ('a -> 'a) -> 'a
val swap : 'a ref -> 'a ref -> unit
val modify : 'a ref -> ('a -> 'a) -> unit
