open Ast;
open PreCast.Syntax;
open LibUtil;
open FanStreamTools;


let apply () = 
  {:lang.extend|Gram
      local: parser_ipatt stream_expr_comp  stream_expr_comp_list
      stream_patt_comp stream_patt_comp_err 
      stream_patt_comp_err_list stream_begin stream_end stream_patt
      parser_case parser_case_list stream_expr stream_quot; 
    expr: Level "top"
        [ "parser";  OPT [ `Uid(n) -> n]  {name}; OPT parser_ipatt{po}; parser_case_list{pcl}
          ->
            match name with
            [ Some o ->
              Ref.protect FanStreamTools.grammar_module_name o (fun _ -> cparser _loc po pcl)
            | None -> cparser _loc po pcl]
        | "match"; S{e}; "with"; "parser";  OPT [`Uid(n) -> n ] {name}; OPT parser_ipatt{po};
          parser_case_list{pcl}
          ->
            match name with
            [ Some o ->
              Ref.protect FanStreamTools.grammar_module_name o (fun _ -> cparser_match _loc e po pcl)
            | None -> cparser_match _loc e po pcl ] ] 
     expr: Level "simple"
     [ stream_begin{name};  stream_end ->
       match name with
       [ Some o ->
         Ref.protect FanStreamTools.grammar_module_name o (fun _ ->
           FanStreamTools.empty _loc )
       | None -> FanStreamTools.empty _loc ]
     | stream_begin{name}; stream_expr_comp_list{sel}; stream_end ->
         match name with
         [ Some o ->   
           Ref.protect FanStreamTools.grammar_module_name o (fun _ -> cstream _loc sel)
         | None -> cstream _loc sel ] ]
     parser_ipatt:
     [ a_lident{i} -> {:patt| $(id:(i:>ident)) |}  | "_" -> {:patt| _ |}  ]         
     parser_case_list:
     [ "["; L0 parser_case SEP "|"{pcl}; "]" -> pcl
     | parser_case{pc} -> [pc] ] 
     parser_case:
     [ "[<"; stream_patt{sp}; stream_end; OPT parser_ipatt{po}; "->"; expr{e}
        ->   (sp, po, e) ] 
     stream_begin: [ "[<"; OPT [ "!"; `Uid(n)->n]{name} -> name  ]   
     stream_end:   [ ">]" -> () ] 
     stream_quot:  [ "'" -> () ]
     stream_expr:  [ expr{e} -> e ] 
     stream_patt:
     [ stream_patt_comp{spc} -> [(spc, None)]
     | stream_patt_comp{spc}; ";"; stream_patt_comp_err_list{sp} ->    [(spc, None) :: sp]
     | -> [] ]
     stream_patt_comp: (* FIXME here *)
     [  patt{p}; OPT [ "when"; stream_expr{e} -> e ]{eo} ->  SpTrm _loc p eo
     | patt{p}; "="; stream_expr{e} -> SpNtr _loc p e
     | stream_quot; patt{p} -> SpStr _loc p ]
    stream_patt_comp_err:
     [ stream_patt_comp{spc};  OPT [ "??"; stream_expr{e} -> e ]{eo } ->  (spc, eo) ] 
    stream_patt_comp_err_list:
     [ stream_patt_comp_err{spc} -> [spc]
     | stream_patt_comp_err{spc}; ";" -> [spc]
     | stream_patt_comp_err{spc}; ";"; stream_patt_comp_err_list{sp} -> [spc :: sp] ] 
    stream_expr_comp_list:
     [ stream_expr_comp{se}; ";"; stream_expr_comp_list{sel} -> [se :: sel]
     | stream_expr_comp{se}; ";" -> [se]
     | stream_expr_comp{se} -> [se] ] 
    stream_expr_comp: 
     [  stream_expr{e} -> SeTrm _loc e
     | stream_quot;stream_expr{e} -> SeNtr _loc e ]  |};

AstParsers.register_parser ("stream",apply)  ;




















