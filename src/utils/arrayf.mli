include module type of Array
val fold_left2 :
    ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a
val stream : 'a array -> 'a Fstream.t
val filter_opt : 'a option array -> 'a array
val filter_map : ('a -> 'b option) -> 'a array -> 'b array
val filter_mapi :(int -> 'h -> 'i option) -> 'h array -> 'i array    
val for_all2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool

