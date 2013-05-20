
open AstLib
let g = Gram.create_lexer
    ~keywords:
    ["derive";
     "unload";
     "clear";
     "keep" ;
     "on";
     "keep";
     "off";
     "show_code";
     "(";
     ")";
     ",";
     ";"
   ]
    ~annot:"derive"
    ();;


{:create|(g:Gram.t)  fan_quot fan_quots|};;

with exp
{:extend|
fan_quot:
  ["derive";"("; L1 [`Lid x -> x | `Uid x  -> x]{plugins}; ")" ->
    (List.iter Typehook.plugin_add plugins; {| () |})
  | "unload"; L1 [`Lid x  -> x | `Uid x -> x ] SEP ","{plugins} ->
      (List.iter Typehook.plugin_remove plugins ; {|() |})
  | "clear" ->
      (FanState.reset_current_filters(); {|()|})
        (* begin Hashtbl.iter (fun _  v -> v.activate <- false) filters; {| |} end *)
  | "keep" ; "on" ->
      (FanState.keep := true; {|() |})
  | "keep" ; "off" -> 
      (FanState.keep := false; {| ()|})
  | "show_code"; "on" ->
      (Typehook.show_code := true; {| () |})
  | "show_code"; "off" ->
      (Typehook.show_code := false; {| ()|})
 ]
  fan_quots:
  [L1[fan_quot{x};";" -> x]{xs} -> seq_sem xs ]
|};;  


begin 
  Syntax.Options.add
    ("-keep",
     (FanArg.Set FanState.keep), "Keep the included type definitions") ;
  Syntax.Options.add
    ("-loaded-plugins",
     (FanArg.Unit Typehook.show_modules), "Show plugins");
end;;
