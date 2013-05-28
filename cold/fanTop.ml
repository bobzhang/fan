open LibUtil

open MkFan

let parse_toplevel_phrase_old = Toploop.parse_toplevel_phrase.contents

let use_file_old = Toploop.parse_use_file.contents

let normal () =
  begin
    Toploop.parse_toplevel_phrase := parse_toplevel_phrase_old;
    Toploop.parse_use_file := use_file_old
  end

let revise () =
  begin
    Toploop.parse_toplevel_phrase :=
      (wrap toplevel_phrase ~print_location:Toploop.print_location);
    Toploop.parse_use_file :=
      (wrap use_file ~print_location:Toploop.print_location)
  end

let _ =
  begin
    Hashtbl.replace Toploop.directive_table "revise"
      (Toploop.Directive_none (fun ()  -> revise ()));
    Hashtbl.replace Toploop.directive_table "normal"
      (Toploop.Directive_none (fun ()  -> normal ()));
    Syntax.current_warning :=
      ((fun loc  txt  ->
          Toploop.print_warning loc Format.err_formatter
            (Warnings.Camlp4 txt)));
    AstParsers.use_parsers ["revise"; "stream"; "macro"]
  end