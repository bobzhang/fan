type t = (* private *) (int * int) list
val max_code : int
val min_code : int
val empty : t
val singleton : int -> t
val is_empty : t -> bool
val interval : int -> int -> t

(* {[ (-1,-1) ]}*)
val eof : t
(* {[ (0,max_code) ]}*)    
val any : t 
val print : Format.formatter -> t -> unit

val dump : t -> unit

val union : t -> t -> t

(* {[
   LexSet.complement [(1,3);(30,40)];
   - : (int * int) list = [ -1-0 4-29 41-1114111 ]
   ]}
 *)
val complement : t -> t

(*
  {[
  LexSet.intersection [(2,4);(40,50)] [(3,49)];
  - : (int * int) list = [ 3-4 40-49 ]
  ]}
 *)    
val intersection : t -> t -> t

(*
  {[
  LexSet.difference [(2,4);(40,50)] [(3,49)];
  - : (int * int) list = [ 2-2 50-50 ]
  ]}
 *)    

val difference :  t -> t -> t 

val norm: (t * 'a) list -> (t * 'a) list

val split: t * (t * 'a list) list ->  t * 'a ->  t * (t * 'a list) list    
val base_char : t 
val ideographic : t 
val combining_char : t 
val digit : t
val extender : t 
val blank : t 
val letter : t 
val tr8876_ident_char : t 
