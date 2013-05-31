
open AstLib


{:new| (g:Fgram.t) fan_quot fan_quots|};;



with exp
{:unsafe_extend|
  fan_quot:
  ["derive";"("; L1 [`Lid x -> x | `Uid x  -> x]{plugins}; ")" ->
    (List.iter Typehook.plugin_add plugins; {| () |})
  | "unload"; L1 [`Lid x  -> x | `Uid x -> x ] SEP ","{plugins} ->
      (List.iter Typehook.plugin_remove plugins ; {|() |})
  | "clear" ->
      (FState.reset_current_filters(); {|()|})
  | "keep" ; "on" ->
      (FState.keep := true; {|() |})
  | "keep" ; "off" -> 
      (FState.keep := false; {| ()|})
  | "show_code"; "on" ->
      (Typehook.show_code := true; {| () |})
  | "show_code"; "off" ->
      (Typehook.show_code := false; {| ()|})
 ]
  fan_quots:
  [L1[fan_quot{x};";" -> x]{xs} -> seq_sem xs ] |};;  


begin 
  Foptions.add
    ("-keep",
     (FArg.Set FState.keep), "Keep the included type definitions") ;
  Foptions.add
    ("-loaded-plugins",
     (FArg.Unit Typehook.show_modules), "Show plugins");
end;;
