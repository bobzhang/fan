type 'a t

val create : 'a -> 'a t

val add : 'a t -> 'a -> unit

(* clean the resize array and return an integral array *)    
val to_array' : 'a t -> 'a array

val get : 'a t -> int -> 'a
    

