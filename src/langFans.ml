



{:new| (g:Fgram.t) fan_quot fan_quots|};;


(* when have local grammars created, g should be specified otherwise, the default
   lexer will mismatch *)
with exp
{:unsafe_extend| (g:Fgram.t)
  fan_quot:
  ["derive";"("; L1 id {plugins}; ")" ->
    (List.iter Typehook.plugin_add plugins)
  | "unload"; L1 id  SEP ","{plugins} ->
      (List.iter Typehook.plugin_remove plugins )
  | "clear" ->
      (FState.reset_current_filters())
  | "keep" ; "on" ->
      (FState.keep := true)
  | "keep" ; "off" -> 
      (FState.keep := false)
  | "show_code"; "on" ->
      (Typehook.show_code := true)
  | "show_code"; "off" ->
      (Typehook.show_code := false)
 ]
  let id:
  [`Lid x -> x | `Uid x -> x]
  let fan_quot_semi:
  [ fan_quot;";" ]
  fan_quots:
  [L1 fan_quot_semi  -> {| ()|} ]

 
|};;  


begin 
  Foptions.add
    ("-keep",
     (FArg.Set FState.keep), "Keep the included type definitions") ;
  Foptions.add
    ("-loaded-plugins",
     (FArg.Unit Typehook.show_modules), "Show plugins");
end;;
