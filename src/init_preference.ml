open Util;;
open Format;;
let fmt = std_formatter
let _loc = Locf.ghost;;
let (!!) = Sys.command;;


let interface_of_string =
  Lexing.from_string |- Parse.interface;;
let implementation_of_string =
  Lexing.from_string |- Parse.implementation;;

let tg_dp () =
  Clflags.dump_parsetree := not !Clflags.dump_parsetree ;;
let pp =fprintf
let f = std_formatter;;
open Fan;;
open Fan_top;;

open Gdefs;;

(* let print_ocaml_loc (fmt:formatter)(loc:Location.t) = ();; *)

open Syntaxf;;
let t e s = Gramlib.parse_string_eoi e  s;;
let normal () = begin
  Toploop.parse_toplevel_phrase := Parse.toplevel_phrase
end;;

let print_tree f x  = pp f "@[%a@]@." Gprint.dump#tree x ;; 
let p_type_declaration  =  Ast_print.default#type_declaration;;

let env = !Toploop.toplevel_env;;
module O = Obj;;
open Astf;;
(* test dump *)
let t_dump s =  t stru s |> Ast2pt.stru;;
let t_meta  = Gramlib.parse_string_eoi Parse_parse.simple;;
open Parse_fan;;

(* #require "inspect";; *)
(* open Inspect;; *)
(* let view a= Dot.dump_to_file "test.dot"a;; *)
#install_printer print_tree;;
#install_printer Gramf.dump;;
let fmt_position = Location_util.fmt_position ~file:false;;
#install_printer fmt_position;;
(* #install_printer p_type_declaration;; *)

#fan;;
