
(** Fan's enhancement for OCaml's poor standard library *)  

val cons : 'a -> 'a list -> 'a list

val failwithf : ('a, unit, string, 'b) format4 -> 'a

val prerr_endlinef : ('a, unit, string, unit) format4 -> 'a

val invalid_argf : ('a, unit, string, 'b) format4 -> 'a

val some : 'a -> 'a option

val none : 'a option

val memoize : ('a -> 'b) -> 'a -> 'b

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

    
(* type 'a id = 'a -> 'a *)
        




module Option :
  sig
    val may : ('a -> unit) -> 'a option -> unit
    val map : ('a -> 'b) -> 'a option -> 'b option
    val bind : ('a -> 'b option) -> 'a option -> 'b option
    val apply : ('a -> 'a) option -> 'a -> 'a
    val filter : ('a -> bool) -> 'a option -> 'a option
    val default : 'a -> 'a option -> 'a
    val is_some : 'a option -> bool
    val is_none : 'a option -> bool
    val get_exn : 'a option -> exn -> 'a
    val get : 'a option -> 'a
    val map_default : ('a -> 'b) -> 'b -> 'a option -> 'b
    val compare : ?cmp:('a -> 'a -> int) -> 'a option -> 'a option -> int
    val eq : ?eq:('a -> 'a -> bool) -> 'a option -> 'a option -> bool
  end
module Buffer :
  sig
    include module type of Buffer with type t = Buffer.t
    val ( +> ) : t -> char -> t
    val ( +>> ) : t -> string -> t
  end
module Array :
  sig
    include module type of Array
    val fold_left2 :
      ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a
    val stream : 'a array -> 'a Fstream.t
    val filter_opt : 'a option array -> 'a array
    val filter_map : ('a -> 'b option) -> 'a array -> 'b array
    val filter_mapi :(int -> 'h -> 'i option) -> 'h array -> 'i array    
    val for_all2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
  end
(* module Fstream : *)
(*   sig *)
(*     include module type of Fstream  with type 'a t = 'a Fstream.t  *)
(*   end *)
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
(* module Unix : *)
(*   sig *)
(*     include module type of Unix *)
(*     val folddir : f:('a -> string -> 'a) -> init:'a -> string -> 'a *)
(*     val try_set_close_on_exec : file_descr -> bool *)
(*     val gen_open_proc_full : *)
(*       string list -> *)
(*       file_descr -> file_descr -> file_descr -> file_descr list -> int *)
(*     val open_process_full : *)
(*       string list -> int * (file_descr * file_descr * file_descr) *)
(*     val open_shell_process_full : *)
(*       string -> int * (file_descr * file_descr * file_descr) *)
(*   end *)

type space_formatter =  (unit, Format.formatter, unit )format

val pp_list : ?sep:space_formatter ->
  ?first:space_formatter ->
  ?last:space_formatter ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
      
val pp_option : ?first:space_formatter ->
  ?last:space_formatter ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit


module Format : sig
  include module type of Format with type formatter = Format.formatter
  val pp_print_list : (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
  val pp_print_int32 : Format.formatter -> int32 -> unit
  val pp_print_int64 : Format.formatter -> int64 -> unit
  val pp_print_nativeint : Format.formatter -> nativeint -> unit
  val pp_print_float : formatter -> float -> unit
  val pp_print_string : Format.formatter -> string -> unit
  val pp_print_bool : formatter -> bool -> unit
  val pp_print_char : formatter -> char -> unit
  val pp_print_unit : Format.formatter -> unit -> unit
  val pp_print_option : (formatter -> 'b -> unit) -> formatter -> 'b option -> unit
end
