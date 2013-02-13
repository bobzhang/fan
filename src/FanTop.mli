val wrap : (FanToken.stream -> 'a) -> Lexing.lexbuf -> 'a


val toplevel_phrase : FanToken.stream -> Parsetree.toplevel_phrase

val use_file : FanToken.stream -> Parsetree.toplevel_phrase list



val revise_parser : Lexing.lexbuf -> Parsetree.toplevel_phrase

val normal : unit -> unit

val revise : unit -> unit

(* val token : unit -> unit *)

(* val fake : *)
(*   ([> FanToken.t ] * 'a) LibUtil.XStream.t -> Parsetree.toplevel_phrase *)
