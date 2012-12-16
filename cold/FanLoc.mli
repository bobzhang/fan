



type position = Lexing.position = {
  pos_fname: string;
  pos_lnum: int;
  pos_bol: int;
  pos_cnum: int;
}
  
type t = Location.t = {
  loc_start: position;
  loc_end: position;
  loc_ghost: bool;
}
  
      
val dump : Format.formatter -> t -> unit
val ghost_name : string
val ghost : t
val mk : string -> t
val of_tuple :
  string * int * int * int * int * int * int * bool -> t
val to_tuple :
  t -> string * int * int * int * int * int * int * bool
val better_file_name : string -> string -> string
val of_lexbuf : Lexing.lexbuf -> t

val start_pos: t -> position
val stop_pos: t -> position
val merge: t -> t -> t
val join: t -> t
val join_end: t -> t    
val map:
  (position -> position) ->
  [< `both | `start | `stop ] -> t -> t
val move_pos: int -> position -> position
val move: [< `both | `start | `stop ] -> int -> t -> t
val move_line: int -> t -> t
val shift: int -> t -> t
val file_name: t -> string
val start_line: t -> int
val stop_line: t -> int
val start_bol: t -> int
val stop_bol: t -> int
val start_off: t -> int
val stop_off: t -> int
val is_ghost: t -> bool
val set_file_name: string -> t -> t
val ghostify: t -> t
val make_absolute: t -> t
val strictly_before: t -> t -> bool
val to_string: t -> string

val print: Format.formatter -> t -> unit
val pp_print_t: Format.formatter -> t -> unit
    
val check: t -> string -> bool
exception Exc_located of t * exn
val name: string ref
val raise: t -> exn -> 'a
val error_report: t * string -> unit
val string_loc: t
val of_positions: position -> position -> t
val dummy_pos: position
