



%new{ (g:Gramf.t) fan_quot fan_quots};;


(* when have local grammars created, g should be specified otherwise, the default
   lexer will mismatch *)
with exp
%unsafe_extend{ (g:Gramf.t)
  fan_quot:
  ["derive";"("; L1 id {plugins}; ")" %{List.iter Typehook.plugin_add plugins}
  | "unload"; L1 id  SEP ","{plugins} %{List.iter Typehook.plugin_remove plugins }
  | "clear" %{State.reset_current_filters()}
  | "keep" ; "on" %{State.keep := true}
  | "keep" ; "off" %{State.keep := false}
  | "show_code"; "on" %{Typehook.show_code := true}
  | "show_code"; "off" %{Typehook.show_code := false}
 ]
  let id:
  [ Lid x  %{x} | Uid x  %{x}]
  let fan_quot_semi:
  [ fan_quot;";" ]
  fan_quots:
  [L1 fan_quot_semi %{ %{ ()}} ]

 
};;  


begin 
  Foptions.add
    ("-keep",
     (Arg.Set State.keep), "Keep the included type definitions") ;
  Foptions.add
    ("-loaded-plugins",
     (Arg.Unit Typehook.show_modules), "Show plugins");
  Ast_quotation.of_exp
    ~name:(Ns.lang, "fans")
    ~entry:fan_quots ();
end;;

(* local variables: *)
(* compile-command: "cd ../main_annot && pmake lang_fans.cmo" *)
(* end: *)
