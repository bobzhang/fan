let parse_toplevel_phrase_old = !Toploop.parse_toplevel_phrase
let use_file_old = !Toploop.parse_use_file
let normal =
  function
  | () ->
      (Toploop.parse_toplevel_phrase := parse_toplevel_phrase_old;
       Toploop.parse_use_file := use_file_old)
let fan =
  function
  | () ->
      (Toploop.parse_toplevel_phrase :=
         (Mktop.wrap Mktop.toplevel_phrase
            ~print_location:Toploop.print_location);
       Toploop.parse_use_file :=
         (Mktop.wrap Mktop.use_file ~print_location:Toploop.print_location))
let _ =
  Hashtbl.replace Toploop.directive_table "fan"
    (Toploop.Directive_none (function | () -> fan ()));
  Hashtbl.replace Toploop.directive_table "normal"
    (Toploop.Directive_none (function | () -> normal ()));
  Hashtbl.replace Toploop.directive_table "pwd"
    (Toploop.Directive_none (function | () -> ignore @@ (Sys.command "pwd")));
  Fan_warnings.current :=
    ((function
      | loc ->
          (function
           | txt ->
               Toploop.print_warning loc Format.err_formatter
                 (Warnings.Preprocessor txt))));
  Ast_parsers.use_parsers ["fan"]
