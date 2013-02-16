val cons : 'a -> 'a list -> 'a list
val failwithf : ('a, unit, string, 'b) format4 -> 'a
val prerr_endlinef : ('a, unit, string, unit) format4 -> 'a
val invalid_argf : ('a, unit, string, 'b) format4 -> 'a
val some : 'a -> 'a option
val none : 'a option
val memoize : ('a -> 'b) -> 'a -> 'b
val finally : (unit -> 'a) -> ('b -> 'c) -> 'b -> 'c
val with_dispose : dispose:('a -> 'b) -> ('a -> 'c) -> 'a -> 'c
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external ( & ) : ('a -> 'b) -> 'a -> 'b = "%apply"
external id : 'a -> 'a = "%identity"
external ( !& ) : 'a -> unit = "%ignore"
val time : ('a -> 'b) -> 'a -> 'b * float
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
type 'a id = 'a -> 'a
module Queue :
  sig
    include module type of Queue with type 'a t = 'a Queue.t
    val find : 'a t -> f:('a -> bool) -> 'a option
    val find_map : 'a t -> f:('a -> 'b option) -> 'b option
    val to_list_rev : 'a t -> 'a list
    val of_list : 'a list -> 'a t
    val rev : 'a t -> 'a t
  end
module List :
  sig
    include module type of List 
    val rev_len : 'a list -> int * 'a list
    val hd : 'a list -> 'a
    val tl : 'a list -> 'a list
    val safe_tl : 'a list -> 'a list
    val null : 'a list -> bool
    val drop : int -> 'a list -> 'a list
    val lastbut1 : 'a list -> 'a list * 'a
    val last : 'a list -> 'a
    val split_at : int -> 'a list -> 'a list * 'a list
    val find_map : ('a -> 'b option) -> 'a list -> 'b
    val fold_lefti : (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> int * 'a
    val remove : 'a -> ('a * 'b) list -> ('a * 'b) list
    val iteri : (int -> 'a -> unit) -> 'a list -> unit
    type dir = [ `Left | `Right ]
    val reduce_left : ('a -> 'a -> 'a) -> 'a list -> 'a
    val reduce_left_with :
      compose:('a -> 'a -> 'a) -> f:('b -> 'a) -> 'b list -> 'a
    val reduce_right_with :
      compose:('a -> 'a -> 'a) -> f:('b -> 'a) -> 'b list -> 'a
    val reduce_right : ('a -> 'a -> 'a) -> 'a list -> 'a
    val init : int -> (int -> 'a) -> 'a list
    val concat_map : ('a -> 'b list) -> 'a list -> 'b list
    val filter_map : ('a -> 'b option) -> 'a list -> 'b list
    val take_rev: int -> 'a list -> 'a list
  end
module type MAP = sig
  include Map.S
  val of_list: (key * 'a) list -> 'a t 
  val of_hashtbl: (key, 'a) Hashtbl.t -> 'a t 
  val elements: 'a t  -> (key * 'a) list 
  val add_list: (key * 'a) list  -> 'a t  ->  'a t
  val find_default: default :'a -> key -> 'a t  -> 'a
  val find_opt : key -> 'a t -> 'a option    
    (* FIXME  [~default:] [~default :] *)
end
      
module type SET = sig
  include Set.S
  val of_list: elt list -> t
  val add_list: t ->  elt list -> t 
  val of_array: elt array -> t
  val add_array: t -> elt array -> t 
end
      
module MapMake :
  functor (S : Map.OrderedType) -> MAP with type key= S.t
module SetMake :
  functor (S : Set.OrderedType) -> SET with type elt = S.t
        

module SMap : MAP with type key = string
module IMap : MAP with type key = int
module ISet : SET with type elt = int
module SSet : SET with type elt = string       
module Hashset :
  sig
    type 'a t = ('a, unit) Hashtbl.t
    val create : ?random:bool -> int -> ('a, 'b) Hashtbl.t
    val add : ('a, unit) Hashtbl.t -> 'a -> unit
    val remove : ('a, 'b) Hashtbl.t -> 'a -> unit
    val mem : ('a, 'b) Hashtbl.t -> 'a -> bool
    val iter : ('a -> unit) -> ('a, unit) Hashtbl.t -> unit
    val fold : ('a -> 'b -> 'b) -> ('a, unit) Hashtbl.t -> 'b -> 'b
    val elements : ('a, 'b) Hashtbl.t -> int
    val clear : ('a, 'b) Hashtbl.t -> unit
    val of_list : ?size:int -> 'a list -> ('a, unit) Hashtbl.t
    val add_list : ('a, unit) Hashtbl.t -> 'a list -> unit
    val to_list : ('a, unit) Hashtbl.t -> 'a list
  end
val mk_set : cmp:('a -> 'a -> int) -> (module Set.S with type elt = 'a)
val mk_map : cmp:('a -> 'a -> int) -> (module Map.S with type key = 'a)
val mk_hashtbl :
  eq:('a -> 'a -> bool) ->
  hash:('a -> int) -> (module Hashtbl.S with type key = 'a)
module Char :
  sig
    include module type of Char
    val is_whitespace : char -> bool
    val is_newline : char -> bool
    val is_digit : char -> bool
    val is_uppercase : char -> bool
    val is_lowercase : char -> bool
  end
module Return :
  sig
    type 'a t = 'a -> exn
    val return : ('a -> exn) -> 'a -> 'b
    val label : ('a t -> 'a) -> 'a
    val with_label : ('a t -> 'a) -> 'a
  end
module String :
  sig
    include module type of String
    val init : int -> (int -> char) -> string
    val is_empty : string -> bool
    val not_empty : string -> bool
    val starts_with : string -> string -> bool
    val ends_with : string -> string -> bool
    val of_char : char -> string
    val drop_while : (char -> bool) -> string -> string
    val neg : string -> string
    val map : (char -> char) -> string -> string
    val lowercase : string -> string
    val find_from : string -> int -> string -> int
    val find : string -> string -> int
    val split : string -> string -> string * string
    val rfind_from : string -> int -> string -> int
    val rfind : string -> string -> int
    val nsplit : string -> string -> string list
  end
module Ref :
  sig
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
  end
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
module Hashtbl :
  sig
    include module type of Hashtbl with type ('a,'b) t = ('a,'b) Hashtbl.t
    val keys : ('a, 'b) t -> 'a list
    val values : ('a, 'b) t -> 'b list
    val find_default : default:'a -> ('b, 'a) t -> 'b -> 'a
    val find_opt : ('b,'a) t -> 'b -> 'a option        
  end
module Array :
  sig
    include module type of Array
    val fold_left2 :
      ('a -> 'b -> 'c -> 'a) -> 'a -> 'b array -> 'c array -> 'a
    val stream : 'a array -> 'a XStream.t
    val filter_opt : 'a option array -> 'a array
    val filter_map : ('a -> 'b option) -> 'a array -> 'b array
    val for_all2 : ('a -> 'b -> bool) -> 'a array -> 'b array -> bool
  end
module XStream :
  sig
    include module type of XStream  with type 'a t = 'a XStream.t 
    val rev : 'a t -> 'a t
    val tail : 'a t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val peek_nth : 'a t -> int -> 'a option
    val dup : 'a t -> 'a t
    val njunk : int -> 'a t -> unit
    val filter : ('a -> bool) -> 'a t -> 'a t
  end
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
module Unix :
  sig
    include module type of Unix
    val folddir : f:('a -> string -> 'a) -> init:'a -> string -> 'a
    val try_set_close_on_exec : file_descr -> bool
    val gen_open_proc_full :
      string list ->
      file_descr -> file_descr -> file_descr -> file_descr list -> int
    val open_process_full :
      string list -> int * (file_descr * file_descr * file_descr)
    val open_shell_process_full :
      string -> int * (file_descr * file_descr * file_descr)
  end

module LStack: sig
  type 'a t = { mutable elts : 'a list; mutable length : int; }
  exception Empty
  val invariant : 'a t -> unit
  val create : unit -> 'a t
  val set : 'a t -> 'a list -> int -> unit
  val push : 'a -> 'a t -> unit
  val pop_exn : 'a t -> 'a
  val pop : 'a t -> 'a option
  val top_exn : 'a t -> 'a
  val top : 'a t -> 'a option
  val clear : 'a t -> unit
  val copy : 'a t -> 'a t
  val length : 'a t -> int
  val is_empty : 'a t -> bool
  val iter : 'a t -> f:('a -> unit) -> unit
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val exists : 'a t -> f:('a -> bool) -> bool
  val for_all : 'a t -> f:('a -> bool) -> bool
  val find_map : 'a t -> f:('a -> 'b option) -> 'b
  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
  val to_array : 'a t -> 'a array
  val until_empty : 'a t -> ('a -> 'b) -> unit
  val topn_rev: int -> 'a t -> 'a list    
end
