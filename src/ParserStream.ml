open Ast;
open Syntax;
open LibUtil;
open FanStreamTools;


let apply () = 
  {:extend|Gram
      local: parser_ipat stream_exp_comp  stream_exp_comp_list
      stream_pat_comp stream_pat_comp_err 
      stream_pat_comp_err_list stream_begin stream_end stream_pat
      parser_case parser_case_list stream_exp stream_quot; 
    exp: Level "top"
        [ "parser";  OPT [ `Uid(n) -> n]  {name}; OPT parser_ipat{po}; parser_case_list{pcl}
          ->
            match name with
            [ Some o ->
              Ref.protect FanStreamTools.grammar_module_name o (fun _ -> cparser _loc po pcl)
            | None -> cparser _loc po pcl]
        | "match"; S{e}; "with"; "parser";  OPT [`Uid(n) -> n ] {name}; OPT parser_ipat{po};
          parser_case_list{pcl}
          ->
            match name with
            [ Some o ->
              Ref.protect FanStreamTools.grammar_module_name o (fun _ -> cparser_match _loc e po pcl)
            | None -> cparser_match _loc e po pcl ] ] 
     exp: Level "simple"
     [ stream_begin{name};  stream_end ->
       match name with
       [ Some o ->
         Ref.protect FanStreamTools.grammar_module_name o (fun _ ->
           FanStreamTools.empty _loc )
       | None -> FanStreamTools.empty _loc ]
     | stream_begin{name}; stream_exp_comp_list{sel}; stream_end ->
         match name with
         [ Some o ->   
           Ref.protect FanStreamTools.grammar_module_name o (fun _ -> cstream _loc sel)
         | None -> cstream _loc sel ] ]
     parser_ipat:
     [ a_lident{i} -> {:pat| $(id:(i:>ident)) |}  | "_" -> {:pat| _ |}  ]         
     parser_case_list:
     [ "["; L0 parser_case SEP "|"{pcl}; "]" -> pcl
     | parser_case{pc} -> [pc] ] 
     parser_case:
     [ "[<"; stream_pat{sp}; stream_end; OPT parser_ipat{po}; "->"; exp{e}
        ->   (sp, po, e) ] 
     stream_begin: [ "[<"; OPT [ "!"; `Uid(n)->n]{name} -> name  ]   
     stream_end:   [ ">]" -> () ] 
     stream_quot:  [ "'" -> () ]
     stream_exp:  [ exp{e} -> e ] 
     stream_pat:
     [ stream_pat_comp{spc} -> [(spc, None)]
     | stream_pat_comp{spc}; ";"; stream_pat_comp_err_list{sp} ->    [(spc, None) :: sp]
     | -> [] ]
     stream_pat_comp: (* FIXME here *)
     [  pat{p}; OPT [ "when"; stream_exp{e} -> e ]{eo} ->  SpTrm _loc p eo
     | pat{p}; "="; stream_exp{e} -> SpNtr _loc p e
     | stream_quot; pat{p} -> SpStr _loc p ]
    stream_pat_comp_err:
     [ stream_pat_comp{spc};  OPT [ "??"; stream_exp{e} -> e ]{eo } ->  (spc, eo) ] 
    stream_pat_comp_err_list:
     [ stream_pat_comp_err{spc} -> [spc]
     | stream_pat_comp_err{spc}; ";" -> [spc]
     | stream_pat_comp_err{spc}; ";"; stream_pat_comp_err_list{sp} -> [spc :: sp] ] 
    stream_exp_comp_list:
     [ stream_exp_comp{se}; ";"; stream_exp_comp_list{sel} -> [se :: sel]
     | stream_exp_comp{se}; ";" -> [se]
     | stream_exp_comp{se} -> [se] ] 
    stream_exp_comp: 
     [  stream_exp{e} -> SeTrm _loc e
     | stream_quot;stream_exp{e} -> SeNtr _loc e ]  |};

AstParsers.register_parser ("stream",apply)  ;




















