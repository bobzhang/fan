
open Fsyntax
(* open FCMacroGen *)


{:create|
  macro_def opt_macro_value uident 
|};;

let apply () = begin 
  {:extend|

    stru: First
    [ macro_def{x} -> FCMacroGen.execute_macro ~exp ~pat {:stru|let _ = () |} (*FIXME*)
        (fun a b -> {:stru| $a;; $b |}) x ]
    macro_def:
    [ "DEFINE"; `Uid i; opt_macro_value{def} -> FCMacroGen.Def i def
    | "UNDEF";  uident{i} -> FCMacroGen.Und i]
    opt_macro_value :
    [ "("; L1 lid SEP ","{pl}; ")"; "="; exp{e} -> Some (pl, e)
    | "="; exp{e} -> Some ([], e)
    | -> None ]
    let lid : [`Lid x -> x ]    

    exp: Level "top"
    [ "DEFINE"; `Lid i; "="; exp{def}; "IN"; exp{body} ->
        (new Exp.subst _loc [(i, def)])#exp body ] 


    let kwd:
    [ `KEYWORD ("DEFINE" | "UNDEF"|"IN" as x) -> x ]
    (* dirty hack to allow polymorphic variants using the introduced keywords.FIXME *)        
    exp: Before "simple"
    [ "`";  kwd{kwd} -> {:exp| $vrn:kwd |}
    | "`"; luident{s} -> {:exp| $vrn:s |} ]
    pat: Before "simple"
    [ "`"; kwd{kwd} -> {:pat| $vrn:kwd |}
    | "`"; luident{s} -> {:pat| $vrn:s |} ] |};
  Foptions.add
    ("-D",
     (FArg.String (FCMacroGen.parse_def ~exp ~pat)  ),
     "<string> Define for IFDEF instruction.");
  Foptions.add
    ("-U",
     (FArg.String (FCMacroGen.undef ~exp ~pat)),
     "<string> Undefine for IFDEF instruction.");
  Foptions.add
    ("-I",
     (FArg.String FIncludeDir.add),
     "<string> Add a directory to INCLUDE search path.");
end;;
    


(* TODO, we need record more information here  *)
AstParsers.register_parser ("macro", apply);;















