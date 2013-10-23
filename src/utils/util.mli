
(** Enhancement to the {!Pervasives} module  *)  

val hash_variant : string -> int
    
val cons : 'a -> 'a list -> 'a list

val failwithf : ('a, unit, string, 'b) format4 -> 'a

val prerr_endlinef : ('a, unit, string, unit) format4 -> 'a

val invalid_argf : ('a, unit, string, 'b) format4 -> 'a

val some : 'a -> 'a option

val none : 'a option

val finally : action:(unit -> 'a) -> 'b -> ('b -> 'c)  -> 'c

val with_dispose : dispose:('a -> 'b)  -> 'a -> ('a -> 'c) -> 'c

external id : 'a -> 'a = "%identity"

external ( !& ) : 'a -> unit = "%ignore"


val ( <| ) : ('a -> 'b) -> 'a -> 'b

val ( |- ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val ( -| ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

val ( *** ) : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd

val ( &&& ) : ('a -> 'b) -> ('a -> 'c) -> 'a -> 'b * 'c

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

val const : 'a -> 'b -> 'a

(** [tap x f] apply f to x and then discard its result  *)
val tap : 'a -> ('a -> 'b) -> 'a








module ErrorMonad :
  sig
    type log = string
    type 'a result = Left of 'a | Right of log
    val return : 'a -> 'a result
    val fail : log -> 'a result
    val ( >>= ) : 'a result -> ('a -> 'b result) -> 'b result
    val bind : 'a result -> ('a -> 'b result) -> 'b result
    val map : ('a -> 'b) -> 'a result -> 'b result
    val ( >>| ) : 'a result -> string * ('a -> 'b result) -> 'b result
    val ( >>? ) : 'a result -> string -> 'a result
    val ( <|> ) : ('a -> 'b result) -> ('a -> 'b result) -> 'a -> 'b result
    val unwrap : ('a -> 'b result) -> 'a -> 'b
    val mapi_m : ('a -> int -> 'b result) -> 'a list -> 'b list result
  end


