

(** Unlinke Camlp4, register your own [stru_parser] is not allowed ,
    it's generatlly a dangerous behavior, since if you do it in-consistently,
    this may result in an in-consistent behavior *) 



open Ast



type 'a parser_fun  =
    ?directive_handler:('a -> 'a option) -> loc
      -> char XStream.t -> 'a option

type 'a printer_fun  =
      ?input_file:string -> ?output_file:string ->
        'a option -> unit
        

(** When  the parser encounter a directive it stops (since the directive may change  the
    syntax), the given [directive_handler] function  evaluates  it  and
    the parsing starts again. *)
val parse_implem : stru parser_fun
(** see [parse_implem]*)
val parse_interf : sigi parser_fun

(** turn the printer to vanilla ocaml output *)
val register_text_printer :  unit -> unit
(** turn the printer to binary parsetree output *)
val register_bin_printer :  unit -> unit     
    



        




module CurrentPrinter : 
  sig
    val print_interf : ?input_file:string -> ?output_file:string ->
      sigi option   -> unit
    val print_implem : ?input_file:string -> ?output_file:string ->
      stru option  -> unit
  end




