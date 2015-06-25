


(** FIXME a better register mode *)


(* avoid dependency on [Parse] module  *)
let parse_toplevel_phrase_old = !Toploop.parse_toplevel_phrase;;
let use_file_old = !Toploop.parse_use_file ;;

let normal () = begin
  Toploop.parse_toplevel_phrase := parse_toplevel_phrase_old;
  Toploop.parse_use_file := use_file_old;
end
    
let fan ()  = begin
  Toploop.parse_toplevel_phrase :=
    Mktop.wrap Mktop.toplevel_phrase ~print_location:Toploop.print_location;
  Toploop.parse_use_file :=
    Mktop.wrap Mktop.use_file ~print_location:Toploop.print_location
end;;

(** For fan's js toplevel *)
(* let () = fan();; *)

begin
  Hashtbl.replace Toploop.directive_table "fan"
    (Toploop.Directive_none (fun () -> fan ()));
  Hashtbl.replace Toploop.directive_table "normal"
    (Toploop.Directive_none (fun () -> normal ()));
  Hashtbl.replace Toploop.directive_table "pwd"
    (Toploop.Directive_none (fun () -> ignore @@ Sys.command "pwd"));
  Fan_warnings.current :=
    (fun loc txt ->
      Toploop.print_warning  loc Format.err_formatter
        (Warnings.Preprocessor txt));
  Ast_parsers.use_parsers ["fan"]
end;;

(*
begin
  Topdirs.dir_install_printer
    Format.std_formatter (Longident.parse "Gramf.dump") ;
  fan ()
end;;
*)





(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/fan_top.cmo" *)
(* end: *)
