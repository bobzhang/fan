open LibUtil

open Ast

module type Id = sig val name : string val version : string end

type ('a,'loc) stream_filter = ('a * 'loc) XStream.t -> ('a * 'loc) XStream.t 

module type ParserImpl =
  sig
    val parse_implem :
      ?directive_handler:(stru -> stru option) ->
        FanLoc.t -> char XStream.t -> stru option
    val parse_interf :
      ?directive_handler:(sigi -> sigi option) ->
        FanLoc.t -> char XStream.t -> sigi option
  end

module type PrinterImpl =
  sig
    val print_interf :
      ?input_file:string -> ?output_file:string -> sigi option -> unit
    val print_implem :
      ?input_file:string -> ?output_file:string -> stru option -> unit
  end

type 'a parser_fun =
  ?directive_handler:('a -> 'a option) -> loc -> char XStream.t -> 'a option 

type 'a printer_fun =
  ?input_file:string -> ?output_file:string -> 'a option -> unit 