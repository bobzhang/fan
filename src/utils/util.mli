
(** Fan's enhancement for OCaml's poor standard library *)  

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

(* val time : ('a -> 'b) -> 'a -> 'b * float *)
val ( <| ) : ('a -> 'b) -> 'a -> 'b

val ( |- ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val ( -| ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

val ( *** ) : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd

val ( &&& ) : ('a -> 'b) -> ('a -> 'c) -> 'a -> 'b * 'c

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

val const : 'a -> 'b -> 'a

val tap : ('a -> 'b) -> 'a -> 'a

val is_even : int -> bool

val pp : Format.formatter -> ('a, Format.formatter, unit) format -> 'a

val to_string_of_printer : (Format.formatter -> 'a -> unit) -> 'a -> string

val zfold_left : ?start:int -> until:int -> acc:'a -> ('a -> int -> 'a) -> 'a

type 'a cont = 'a -> exn

val callcc : ('a cont -> 'a) -> 'a

type 'a return = { return : 'b. 'a -> 'b; }

val with_return : ('a return -> 'a) -> 'a

    

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


