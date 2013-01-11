(* open Ast; *)
open PreCast.Syntax;
open FanMacroTools;
open Lib;

{:extend.create|Gram
  macro_def macro_def_sig uident_eval_ifdef uident_eval_ifndef
  else_macro_def else_macro_def_sig else_expr smlist_then smlist_else sglist_then
  sglist_else endif opt_macro_value uident 
|};

let apply () = begin 
  {:extend|Gram

    str_item: First
    [ macro_def{x} -> execute_macro ~expr ~patt {:str_item||} (fun a b -> {:str_item| $a; $b |}) x ]
    sig_item: First
    [ macro_def_sig{x} -> execute_macro ~expr ~patt {:sig_item||} (fun a b -> {:sig_item| $a; $b |}) x ]
    macro_def:
    [ "DEFINE"; uident{i}; opt_macro_value{def} -> Def i def
    | "UNDEF";  uident{i} -> Und i
    | "IFDEF";  uident_eval_ifdef;  "THEN"; smlist_then{st1}; else_macro_def{st2} ->
        make_ITE_result st1 st2
    | "IFNDEF"; uident_eval_ifndef; "THEN"; smlist_then{st1}; else_macro_def{st2} ->
        make_ITE_result st1 st2
    | "INCLUDE"; `STR (_, fname) -> Lazy (lazy (parse_include_file str_items fname)) ]
      
    macro_def_sig:
    [ "DEFINE"; uident{i} -> Def i None
    | "UNDEF";  uident{i} -> Und i
    | "IFDEF";  uident_eval_ifdef;  "THEN"; sglist_then{sg1}; else_macro_def_sig{sg2} ->
        make_ITE_result sg1 sg2
    | "IFNDEF"; uident_eval_ifndef; "THEN"; sglist_then{sg1}; else_macro_def_sig{sg2} ->
        make_ITE_result sg1 sg2
    | "INCLUDE"; `STR (_, fname) ->
        Lazy (lazy (parse_include_file sig_items fname)) ]

    uident_eval_ifdef:
    [ uident{i} -> Stack.push (is_defined i) stack ]
    uident_eval_ifndef:
    [ uident{i} -> Stack.push (not (is_defined i)) stack ]
    else_macro_def:
    [ "ELSE"; smlist_else{st}; endif -> st | endif -> [] ]
    else_macro_def_sig:
    [ "ELSE"; sglist_else{st}; endif -> st | endif -> [] ]
    else_expr:
    [ "ELSE"; expr{e}; endif -> e | endif -> {:expr| () |} ]
    smlist_then:
    [ L1
        [ macro_def{d}; semi ->
          execute_macro_if_active_branch ~expr ~patt _loc
            {:str_item||} (fun a b -> {:str_item| $a; $b |}) Then d
        | str_item{si}; semi -> Str si ]{sml} -> sml ]
    smlist_else:
    [ L1 [ macro_def{d}; semi ->
           execute_macro_if_active_branch ~expr ~patt  _loc
           {:str_item||} (fun a b -> {:str_item| $a; $b |}) Else d
         | str_item{si}; semi -> Str si ]{sml} -> sml ]
    sglist_then:
    [ L1 [ macro_def_sig{d}; semi ->
           execute_macro_if_active_branch ~expr ~patt
          _loc {:sig_item||} (fun a b -> {:sig_item| $a; $b |}) Then d
           | sig_item{si}; semi -> Str si ]{sgl} -> sgl ]   
    sglist_else:
    [ L1 [ macro_def_sig{d}; semi ->
             execute_macro_if_active_branch ~expr ~patt
               _loc {:sig_item||} (fun a b -> {:sig_item| $a; $b |}) Else d
    | sig_item{si}; semi -> Str si ]{sgl} -> sgl ]  
    endif: [ "END" -> () | "ENDIF" -> () ]
    opt_macro_value:
    [ "("; L1 [ `Lid x -> x ] SEP ","{pl}; ")"; "="; expr{e} -> Some (pl, e)
    | "="; expr{e} -> Some ([], e)
    | -> None ]

    expr: Level "top"
    [ "IFDEF"; uident{i}; "THEN"; expr{e1}; else_expr{e2} ->
      if is_defined i then e1 else e2
    | "IFNDEF"; uident{i}; "THEN"; expr{e1}; else_expr{e2} ->
        if is_defined i then e2 else e1
    | "DEFINE"; `Lid i; "="; expr{def}; "IN"; expr{body} ->
        (new Expr.subst _loc [(i, def)])#expr body ] 
    patt:
    [ "IFDEF"; uident{i}; "THEN"; patt{p1};  "ELSE"; patt{p2}; endif ->
      if is_defined i then p1 else p2
    | "IFNDEF"; uident{i}; "THEN"; patt{p1}; "ELSE"; patt{p2}; endif ->
        if is_defined i then p2 else p1 ]
    uident:
    [ `Uid i -> i ]
    (* dirty hack to allow polymorphic variants using the introduced keywords.FIXME *)

    expr: Before "simple"
    [ "`";  [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF"| "DEFINE" | "IN" ]{kwd}
      -> {:expr| `$uid:kwd |}
    | "`"; a_ident{s} -> {:expr| ` $s |} ]

    patt: Before "simple"
    [ "`"; [ "IFDEF" | "IFNDEF" | "THEN" | "ELSE" | "END" | "ENDIF" ]{kwd} ->
      {:patt| `$uid:kwd |}
    | "`"; a_ident{s} -> {:patt| ` $s |} ] |};
  Options.add
    ("-D",
     (FanArg.String (parse_def ~expr ~patt)  ),
     "<string> Define for IFDEF instruction.");
  Options.add
    ("-U",
     (FanArg.String (undef ~expr ~patt)),
     "<string> Undefine for IFDEF instruction.");
  Options.add
    ("-I",
     (FanArg.String add_include_dir),
     "<string> Add a directory to INCLUDE search path.");
end;
    


(* TODO, we need record more information here  *)
AstParsers.register_parser ("macro", apply);















