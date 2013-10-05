
type 'a t = 'a -> exn

val return : 'a t -> 'a -> 'b

val label : ('a t -> 'a) -> 'a

val with_label : ('a t -> 'a) -> 'a
    
