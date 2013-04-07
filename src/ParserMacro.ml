
open PreCast.Syntax;
open FanMacroTools;


{:create|Gram
  macro_def (* macro_def_sig *) uident_eval_ifdef uident_eval_ifndef
  else_macro_def (* else_macro_def_sig *) else_exp smlist_then smlist_else (* sglist_then *)
  (* sglist_else *) endif opt_macro_value uident 
|};

let apply () = begin 
  {:extend|Gram

    stru: First
    [ macro_def{x} -> execute_macro ~exp ~pat {:stru|let _ = () |} (*FIXME*)
        (fun a b -> {:stru| $a; $b |}) x ]
    (* sig_item: First *)
    (* [ macro_def_sig{x} -> *)
    (*   execute_macro ~exp ~pat {:sig_item||} (fun a b -> {:sig_item| $a; $b |}) x ] *)
    macro_def:
    [ "DEFINE"; uident{i}; opt_macro_value{def} -> Def i def
    | "UNDEF";  uident{i} -> Und i
    | "IFDEF";  uident_eval_ifdef;  "THEN"; smlist_then{st1}; else_macro_def{st2} ->
        make_ITE_result st1 st2
    | "IFNDEF"; uident_eval_ifndef; "THEN"; smlist_then{st1}; else_macro_def{st2} ->
        make_ITE_result st1 st2
    | "INCLUDE"; `STR (_, fname) -> Lazy (lazy (FanBasic.parse_include_file strus fname)) ]
      
    (* macro_def_sig: *)
    (* [ "DEFINE"; uident{i} -> Def i None *)
    (* | "UNDEF";  uident{i} -> Und i *)
    (* | "IFDEF";  uident_eval_ifdef;  "THEN"; sglist_then{sg1}; else_macro_def_sig{sg2} -> *)
    (*     make_ITE_result sg1 sg2 *)
    (* | "IFNDEF"; uident_eval_ifndef; "THEN"; sglist_then{sg1}; else_macro_def_sig{sg2} -> *)
    (*     make_ITE_result sg1 sg2 *)
    (* | "INCLUDE"; `STR (_, fname) -> *)
    (*     Lazy (lazy (FanBasic.parse_include_file sig_items fname)) ] *)

    uident_eval_ifdef:
    [ uident{i} -> Stack.push (is_defined i) stack ]
    uident_eval_ifndef:
    [ uident{i} -> Stack.push (not (is_defined i)) stack ]
    else_macro_def:
    [ "ELSE"; smlist_else{st}; endif -> st | endif -> [] ]
    (* else_macro_def_sig: *)
    (* [ "ELSE"; sglist_else{st}; endif -> st | endif -> [] ] *)
    else_exp:
    [ "ELSE"; exp{e}; endif -> e | endif -> {:exp| () |} ]
    smlist_then:
    [ L1
        [ macro_def{d}; ";" ->
          execute_macro_if_active_branch ~exp ~pat _loc
            {:stru|let _ = ()|} (* FIXME *)
            (fun a b -> {:stru| $a; $b |}) Then d
        | stru{si}; ";" -> Str si ]{sml} -> sml ]
    smlist_else:
    [ L1 [ macro_def{d}; ";" ->
           execute_macro_if_active_branch ~exp ~pat  _loc
           {:stru|let _ = ()|} (*FIXME*) (fun a b -> {:stru| $a; $b |}) Else d
         | stru{si}; ";" -> Str si ]{sml} -> sml ]
    (* sglist_then: *)
    (* [ L1 [ macro_def_sig{d}; semi -> *)
    (*        execute_macro_if_active_branch ~exp ~pat *)
    (*       _loc {:sig_item||} (fun a b -> {:sig_item| $a; $b |}) Then d *)
    (*        | sig_item{si}; semi -> Str si ]{sgl} -> sgl ]    *)
    (* sglist_else: *)
    (* [ L1 [ macro_def_sig{d}; semi -> *)
    (*          execute_macro_if_active_branch ~exp ~pat *)
    (*            _loc {:sig_item||} (fun a b -> {:sig_item| $a; $b |}) Else d *)
    (* | sig_item{si}; semi -> Str si ]{sgl} -> sgl ]   *)
    endif: [ "END" -> () | "ENDIF" -> () ]
    opt_macro_value:
    [ "("; L1 [ `Lid x -> x ] SEP ","{pl}; ")"; "="; exp{e} -> Some (pl, e)
    | "="; exp{e} -> Some ([], e)
    | -> None ]

    exp: Level "top"
    [ "IFDEF"; uident{i}; "THEN"; exp{e1}; else_exp{e2} ->
      if is_defined i then e1 else e2
    | "IFNDEF"; uident{i}; "THEN"; exp{e1}; else_exp{e2} ->
        if is_defined i then e2 else e1
    | "DEFINE"; `Lid i; "="; exp{def}; "IN"; exp{body} ->
        (new Exp.subst _loc [(i, def)])#exp body ] 
    pat:
    [ "IFDEF"; uident{i}; "THEN"; pat{p1};  "ELSE"; pat{p2}; endif ->
      if is_defined i then p1 else p2
    | "IFNDEF"; uident{i}; "THEN"; pat{p1}; "ELSE"; pat{p2}; endif ->
        if is_defined i then p2 else p1 ]
    uident:
    [ `Uid i -> i ]
    (* dirty hack to allow polymorphic variants using the introduced keywords.FIXME *)

    exp: Before "simple"
    [ "`";  [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF"| "DEFINE" | "IN" ]{kwd}
      -> {:exp| $vrn:kwd |}
    | "`"; luident{s} -> {:exp| $vrn:s |} ]

    pat: Before "simple"
    [ "`"; [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF" ]{kwd} ->
      {:pat| $vrn:kwd |}
    | "`"; luident{s} -> {:pat| $vrn:s |} ] |};
  Options.add
    ("-D",
     (FanArg.String (parse_def ~exp ~pat)  ),
     "<string> Define for IFDEF instruction.");
  Options.add
    ("-U",
     (FanArg.String (undef ~exp ~pat)),
     "<string> Undefine for IFDEF instruction.");
  Options.add
    ("-I",
     (FanArg.String FanBasic.add_include_dir),
     "<string> Add a directory to INCLUDE search path.");
end;
    


(* TODO, we need record more information here  *)
AstParsers.register_parser ("macro", apply);















