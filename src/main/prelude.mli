

(** Unlinke Camlp4, register your own [stru_parser] is not allowed ,
    it's generatlly a dangerous behavior, since if you do it in-consistently,
    this may result in an in-consistent behavior *) 

open FAst

(** type [parser_fun] defines the interface for Fan's parser
    for example, the exported function [parse_implem] has type
    [stru parse_fun]
 *)
type 'a parser_fun  =
     loc -> char Streamf.t -> 'a option


(** type [printer_fun] define the interface for the Fan's printer
    The [input_file] is required for the backend to mark its original source
    [input_file] is specified by flag [-impl], the major name if not
    [output_file] is specified by flag [-o], stdout if not
 *)         
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
(* val register_text_printer :  unit -> unit *)

(** turn the printer to binary parsetree output *)
(* val register_bin_printer :  unit -> unit      *)
    
(** make use of OCaml's [-dsource] option *)
(* val register_parsetree_printer : unit -> unit *)

val stru_printer :
    (?input_file:string -> ?output_file:string -> FAst.stru option -> unit) ref

val sigi_printer :
    (?input_file:string -> ?output_file:string -> FAst.sigi option -> unit) ref
    
module CurrentPrinter : 
  sig
    (** the last argument is the [ast] to be printed, if it is None, then
        generally it will print nothing *)
    val print_interf : sigi printer_fun
    val print_implem : stru printer_fun
  end



val backends : (string, stru printer_fun * sigi printer_fun) Hashtbl.t

(** {3 functions for toplevel} *)


val wrap :
    'a Tokenf.parse  -> print_location:(Format.formatter -> Locf.t -> unit) ->
        Lexing.lexbuf -> 'a    

val toplevel_phrase : Parsetree.toplevel_phrase Tokenf.parse


(** Here we only *intercept* directives [at the beginning] of the file. 
    toplevel has a differen semantics for [use_file] *)
val use_file : Parsetree.toplevel_phrase list Tokenf.parse
