

(** Unlinke Camlp4, register your own [stru_parser] is not allowed ,
    it's generatlly a dangerous behavior, since if you do it in-consistently,
    this may result in an in-consistent behavior *) 

open FAst

type 'a parser_fun  =
     loc -> char XStream.t -> 'a option

type 'a printer_fun  =
      ?input_file:string -> ?output_file:string ->
        'a option -> unit
        

(** When  the parser encounter a directive it stops
    (since the directive may change  the
    syntax), the given [directive_handler] function  evaluates  it  and
    the parsing starts again. *)
val parse_implem : stru parser_fun


(** see [parse_implem]*)
val parse_interf : sigi parser_fun


(** it will pass [directive_handler] to [parse_file] *)    
val parse_file :
      string -> 'a parser_fun -> 'a option

    
(** turn the printer to vanilla ocaml output *)
val register_text_printer :  unit -> unit

(** turn the printer to binary parsetree output *)
val register_bin_printer :  unit -> unit     
    



        




module CurrentPrinter : 
  sig
    (** the last argument is the [ast] to be printed, if it is None, then
        generally it will print nothing *)
    val print_interf : ?input_file:string -> ?output_file:string ->
      sigi option   -> unit
    val print_implem : ?input_file:string -> ?output_file:string ->
      stru option  -> unit
  end





(** {3 functions for toplevel} *)


val wrap :
    'a Ftoken.parse  -> print_location:(Format.formatter -> FLoc.t -> unit) ->
        Lexing.lexbuf -> 'a    

val toplevel_phrase : Parsetree.toplevel_phrase Ftoken.parse


(** Here we only *intercept* directives [at the beginning] of the file. 
    toplevel has a differen semantics for [use_file] *)
val use_file : Parsetree.toplevel_phrase list Ftoken.parse
