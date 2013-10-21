
(** the location module *)

(** Be careful to rename the file,
    change the compiler first before
    you change the name   *)

open Format

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
  
val pp_print_position: formatter -> position -> unit      

val ghost_name : string

val ghost : t

val mk : string -> t

val merge : t -> t -> t

val file_name : t -> string

val is_ghost : t -> bool

val set_file_name : string -> t -> t
val ghostify : t -> t
val make_absolute : t -> t

val strictly_before : t -> t -> bool

val to_string : t -> string

val print : t Formatf.t

val pp_print_t : t Formatf.t 
    
val check : t -> string -> bool
    
exception Exc_located of t * exn

val name : string ref

val raise : t -> exn -> 'a

val error_report: t * string -> unit

val string_loc: t


(** raise [Failure] exception *)    
val failf: t -> ('a, unit, string, 'b) format4 -> 'a

module Ops : sig
  val (<+>): t -> t -> t     
end
