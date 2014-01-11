
(** a list of intervals *)  
type t = (int * int) list
val meta_t : Locf.t -> t ->  Astf.ep 
val empty : t

val is_empty : t -> bool

val singleton : int -> t 

val interval : int -> int -> t 

val union : t -> t -> t 

val inter : t -> t -> t 
val diff : t -> t -> t

(** 256 *)
val eof : t 

val all_chars : t 

(** all chars including eof *)    
val all_chars_eof : t 

val complement : t -> t 

val env_to_array : (t * 'a) list -> 'a array

val to_string : t -> string
