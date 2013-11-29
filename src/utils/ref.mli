



(** @params r v body
treat [r]'s state as [v] in [body], the value will be restored when exit *)
val protect : 'a ref -> 'a -> (unit -> 'b) -> 'b

(** see [protect]*)
val protect2 : 'a ref * 'a -> 'b ref * 'b -> (unit -> 'c) -> 'c

val protects : 'a ref list -> 'a list -> (unit -> 'b) -> 'b    


(** A weak form of [protect]. Restore the value when exit *)    
val save : 'a ref -> (unit -> 'b) -> 'b

val save2 : 'a ref -> 'b ref -> (unit -> 'c) -> 'c

val saves : 'a ref list -> (unit -> 'b) -> 'b


val post : 'a ref -> ('a -> 'a) -> 'a

val pre : 'a ref -> ('a -> 'a) -> 'a

val swap : 'a ref -> 'a ref -> unit

val modify : 'a ref -> ('a -> 'a) -> unit
