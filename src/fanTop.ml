
open LibUtil

(** FIXME a better register mode *)
open MkFan;;

(* avoid dependency on [Parse] module  *)
let parse_toplevel_phrase_old = !Toploop.parse_toplevel_phrase;;
let use_file_old = !Toploop.parse_use_file ;;

let normal () = begin
  Toploop.parse_toplevel_phrase := parse_toplevel_phrase_old;
  Toploop.parse_use_file := use_file_old;
end
    
let revise ()  = begin
  Toploop.parse_toplevel_phrase :=
    wrap toplevel_phrase ~print_location:Toploop.print_location;
  Toploop.parse_use_file :=
    wrap use_file ~print_location:Toploop.print_location
end;;

begin 
  Hashtbl.replace Toploop.directive_table "revise"
    (Toploop.Directive_none (fun () -> revise ()));
  Hashtbl.replace Toploop.directive_table "normal"
    (Toploop.Directive_none (fun () -> normal ()));

  Syntax.current_warning :=
    (fun loc txt ->
      Toploop.print_warning  loc Format.err_formatter
        (Warnings.Camlp4 txt));
  AstParsers.use_parsers
    ["revise";"stream";"macro"]
end;;





(* Gram.dump Format.std_formatter Syntax.exp;; *)



(* let token() = begin *)
(*   Toploop.parse_toplevel_phrase := wrap fake ; *)
(* end; *)
(* let fake token_stream = begin  *)
(*   try *)
(*     XStream.iter (fun (tok,_) -> *)
(*       if tok= `INT (3,"3") then raise Not_found *)
(*       else *)
(*         Format.fprintf Format.std_formatter *)
(*           "@[%a@]@." FToken.print tok ) token_stream *)
(*   with *)
(*     [Not_found -> ()]; *)
(*   prerr_endline "got it"; *)
(*   Parsetree.Ptop_dir "pwd" Parsetree.Pdir_none; *)
(* end; *)

