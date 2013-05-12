open Ast
open Syntax
open LibUtil
open FanStreamTools


{:create| Gram
parser_ipat stream_exp_comp  stream_exp_comp_list
      stream_pat_comp stream_pat_comp_err 
      stream_pat_comp_err_list stream_begin
  
      stream_pat
      parser_case parser_case_list
  stream_exp
|}

  
let apply () = 
  {:extend|
    exp : Level "top"
        [ "parser";  OPT [ `Uid(n) -> n]  {name}
            ; OPT parser_ipat{po}
            ; parser_case_list{pcl}
          ->
            (match name with
            | Some o ->
              Ref.protect FanStreamTools.grammar_module_name o (fun _ -> cparser _loc po pcl)
            | None -> cparser _loc po pcl)
        | "match"; S{e}; "with"; "parser";  OPT [`Uid(n) -> n ] {name}; OPT parser_ipat{po};
          parser_case_list{pcl}
          ->
            match name with
            | Some o ->
              Ref.protect FanStreamTools.grammar_module_name o
                  (fun _ -> cparser_match _loc e po pcl)
            | None -> cparser_match _loc e po pcl ]

    stream_exp :
    ["!"; `Uid(n) ->
      Ref.protect FanStreamTools.grammar_module_name n (fun _ ->
           FanStreamTools.empty _loc )
    |  "!"; `Uid(n); stream_exp_comp_list{sel}  ->
        Ref.protect FanStreamTools.grammar_module_name n (fun _ -> cstream _loc sel)
    | stream_exp_comp_list{sel} -> cstream _loc sel
    |  ->  FanStreamTools.empty _loc]

    
     parser_ipat :
     [ a_lident{i} -> (i: alident:> pat)
     | "_" -> {:pat| _ |}  ]         

     parser_case_list :
     [ (* "["; L0 parser_case SEP "|"{pcl}; "]" -> pcl *)
     (* | *)
       "|"; L0 parser_case SEP "|"{pcl} -> pcl
     | parser_case{pc} -> [pc] ]
    
    parser_case :
    [ (* "[<"; *)
      stream_pat{sp}; (* ">]"; OPT parser_ipat{po}; *) "->"; exp{e}
      ->   (sp, None, e) ] 
    stream_begin :
    [ "[<"; OPT [ "!"; `Uid(n)->n]{name} -> name  ]   

    stream_pat :
    [ stream_pat_comp{spc} -> [(spc, None)]
    | stream_pat_comp{spc}; ";"; stream_pat_comp_err_list{sp} -> (spc, None) :: sp
    | -> [] ]


    stream_pat_comp : (* FIXME here *)
    [ pat{p}; "when"; exp{e}  ->  SpWhen (_loc, p, (Some e))
    | pat{p} -> SpWhen (_loc, p, None)
    | pat{p}; "="; exp{e} -> SpMatch (_loc, p, e)
    | "'"; pat{p} -> SpStr (_loc, p) ]
    stream_pat_comp_err :
    [ stream_pat_comp{spc};   "??"; exp{e}->  (spc, Some e)
    | stream_pat_comp{spc} -> (spc,None)] 

    stream_pat_comp_err_list :
    [ stream_pat_comp_err{spc} -> [spc]
    | stream_pat_comp_err{spc}; ";" -> [spc]
    | stream_pat_comp_err{spc}; ";"; stream_pat_comp_err_list{sp} -> spc :: sp ]

    stream_exp_comp : 
    [  exp{e} -> SeTrm _loc e
    | "'";exp{e} -> SeNtr _loc e ]

    stream_exp_comp_list :
    [ stream_exp_comp{se}; ";"; stream_exp_comp_list{sel} -> se :: sel
    | stream_exp_comp{se}; ";" -> [se]
    | stream_exp_comp{se} -> [se] ] 
|};;

AstParsers.register_parser ("stream",apply)  ;;




















