


let rec token = %lex_fan{
 | @whitespace %{token lexbuf}
 | @ocaml_comment %{token lexbuf}
 | @ocaml_lid("derive"|"unload"|"clear"|"keep"|"on"|"off"|"show_code")
 (* | @ocaml_string *)
 | @ocaml_uid
 | @kwd_symbol("("|")"|","|";")
 | @ocaml_eof
 | @default
};;

%create{fan_quot fan_quots};;


(* when have local grammars created, g should be specified otherwise, the default
   lexer will mismatch *)
with exp
%extend{
  fan_quot:
  ["derive";"("; L1 id as plugins; ")" %{List.iter Typehook.plugin_add plugins}
  | "unload"; L1 id  SEP "," as plugins %{List.iter Typehook.plugin_remove plugins }
  | "clear" %{State.reset_current_filters()}
  | "keep" ; "on" %{State.keep := true}
  | "keep" ; "off" %{State.keep := false}
  | "show_code"; "on" %{Typehook.show_code := true}
  | "show_code"; "off" %{Typehook.show_code := false}
 ]
  id@Local:
  [ Lid x  %{x} | Uid x  %{x}]
  fan_quot_semi@Local:
  [ fan_quot;";" ]
  fan_quots:
  [L1 fan_quot_semi %{ %{ ()}} ]

 
};;  

let lexer = Lexing_util.adapt_to_stream token ;;
begin 
  Options.add
    ("-keep",
     (Arg.Set State.keep), "Keep the included type definitions") ;
  Options.add
    ("-loaded-plugins",
     (Arg.Unit Typehook.show_modules), "Show plugins");
  Ast_quotation.of_exp
    ~name:{domain = Ns.lang; name =  "fans"}
    ~lexer
    ~entry:fan_quots ();
end;;

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lang_fans.cmo" *)
(* end: *)
