let parse_toplevel_phrase_old = !Toploop.parse_toplevel_phrase
let use_file_old = !Toploop.parse_use_file
let normal () =
  Toploop.parse_toplevel_phrase := parse_toplevel_phrase_old;
  Toploop.parse_use_file := use_file_old
let fan () =
  Toploop.parse_toplevel_phrase :=
    (Mktop.wrap Mktop.toplevel_phrase ~print_location:Toploop.print_location);
  Toploop.parse_use_file :=
    (Mktop.wrap Mktop.use_file ~print_location:Toploop.print_location)
let _ =
  Hashtbl.replace Toploop.directive_table "fan"
    (Toploop.Directive_none (fun ()  -> fan ()));
  Hashtbl.replace Toploop.directive_table "normal"
    (Toploop.Directive_none (fun ()  -> normal ()));
  Hashtbl.replace Toploop.directive_table "pwd"
    (Toploop.Directive_none (fun ()  -> ignore @@ (Sys.command "pwd")));
  Fan_warnings.current :=
    ((fun loc  txt  ->
        Toploop.print_warning loc Format.err_formatter (Warnings.Camlp4 txt)));
  Ast_parsers.use_parsers ["revise"]
let _ =
  Topdirs.dir_install_printer Format.std_formatter
    (Longident.Ldot ((Longident.Lident "Gramf"), "dump"));
  fan ()
