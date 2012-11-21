type t = private { count : int; data : data; }
and data = private
  |  Sempty
  | Scons of char * data
  | Sapp of data * t
  | Slazy of t Lazy.t
  | Sgen of gen
  | Sbuffio of buffio
and gen = private { mutable curr : char option option; func : int -> char option; }
and buffio = private {
    ic : in_channel;
    buff : string;
    mutable len : int;
    mutable ind : int;
  }
exception Failure
exception Error of string
external count : t -> int = "%field0"
external set_count : t -> int -> unit = "%setfield0"
val set_data : t -> data -> unit
(* val fill_buff : buffio -> unit *)
(* val get_data : t -> data -> data *)
val peek : t -> char option
val junk : t -> unit
val nget : int -> t -> char list * data * int
val npeek : int -> t -> char list
val next : t -> char
val empty : t -> unit
val iter : (char -> 'a) -> t -> unit
val from : (int -> char option) -> t
val of_list : char list -> t
val of_string : string -> t
val of_channel : in_channel -> t
val ising : char -> t
val icons : char -> t -> t
val iapp : t -> t -> t
val sempty : t
val slazy : (unit -> t) -> t
val lsing : (unit -> char) -> t
val lcons : (unit -> char) -> t -> t
val lapp : (unit -> t) -> t -> t
val dump : (char -> 'a) -> t -> unit
val dump_data : (char -> 'a) -> data -> unit
