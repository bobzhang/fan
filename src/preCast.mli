
open Sig
open Ast



val iter_and_take_callbacks : ((string * (unit -> unit)) -> unit) -> unit 


(* val register_parser : *)
(*     stru parser_fun  -> sigi parser_fun  -> unit *)
(* val current_parser : *)
(*     unit -> ( stru parser_fun * sigi parser_fun ) *)

val register_stru_printer : stru printer_fun -> unit

val register_sigi_printer : sigi printer_fun  -> unit

val register_text_printer :  unit -> unit

val register_bin_printer :  unit -> unit     
    
val register_printer : stru printer_fun  ->  sigi printer_fun -> unit

val current_printer : unit -> ( stru printer_fun *  sigi printer_fun)
        
(* val declare_dyn_module : string -> (unit -> unit) -> unit   *)

module CurrentParser : ParserImpl

module CurrentPrinter : PrinterImpl



(* for dynamic loading *)
(* include Sig.PRECAST *)
(* module Syntax     : module type of Syntax  *)
(* val plugin : (module Id) -> (module PLUGIN) -> unit  *)
(* val syntax_plugin:(module Id) -> (module SyntaxPlugin) -> unit *)
(* val syntax_extension: (module Id) -> (module SyntaxExtension) -> unit *)
(* val printer_plugin: (module Id) -> (module PrinterPlugin) -> unit *)
(* val replace_printer: (module Id) -> (module PrinterImpl) -> unit *)
(* val replace_parser: (module Id) -> (module ParserImpl) -> unit *)
(* val parser_plugin: (module Id) -> (module ParserPlugin) -> unit *)
(* val enable_null_printer: unit -> unit *)
(* val enable_auto: (unit->bool) -> unit *)
(* val loaded_modules : string list ref  *)
(* val enable_ocaml_printer : unit -> unit *)

(* val enable_dump_ocaml_ast_printer : unit -> unit *)

(* val enable_dump_ast_printer : unit -> unit *)
(** *)
(* val register_stru_parser : stru parser_fun -> unit *)

(* val register_sigi_parser : sigi parser_fun  -> unit *)

