open LibUtil
open Mktop
let parse_toplevel_phrase_old = Toploop.parse_toplevel_phrase.contents
let use_file_old = Toploop.parse_use_file.contents
let normal () =
  Toploop.parse_toplevel_phrase := parse_toplevel_phrase_old;
  Toploop.parse_use_file := use_file_old
let fan () =
  Toploop.parse_toplevel_phrase :=
    (wrap toplevel_phrase ~print_location:Toploop.print_location);
  Toploop.parse_use_file :=
    (wrap use_file ~print_location:Toploop.print_location)
let _ =
  Printexc.register_printer Mktop.normal_handler;
  Hashtbl.replace Toploop.directive_table "fan"
    (Toploop.Directive_none (fun ()  -> fan ()));
  Hashtbl.replace Toploop.directive_table "normal"
    (Toploop.Directive_none (fun ()  -> normal ()));
  Fsyntax.current_warning :=
    ((fun loc  txt  ->
        Toploop.print_warning loc Format.err_formatter (Warnings.Camlp4 txt)));
  AstParsers.use_parsers ["revise"; "stream"]
let _ =
  Topdirs.dir_install_printer Format.std_formatter
    (Longident.Ldot ((Longident.Lident "Fgram"), "dump"));
  fan ()