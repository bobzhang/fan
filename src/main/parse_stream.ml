open FAst
open! Fsyntax

open Compile_stream


%create{ Fgram
  parser_ipat stream_exp_comp  stream_exp_comp_list
  stream_pat_comp stream_pat_comp_err 
  stream_pat_comp_err_list
  stream_pat parser_case parser_case_list stream_exp
}
  
let apply () = 
  %extend2{
    let  uid: [`Uid(n) %{n}]
    exp : Level "top"
        [ "parser";  OPT uid  {name}
            ; OPT parser_ipat{po}
            ; parser_case_list{pcl} %{
          match name with
          | Some o ->
              Ref.protect Compile_stream.grammar_module_name o (fun _ -> cparser _loc po pcl)
          | None -> cparser _loc po pcl}
        | "match"; S{e}; "with"; "parser";  OPT uid {name}; OPT parser_ipat{po};
          parser_case_list{pcl}
          %{
          match name with
          | Some o ->
              Ref.protect Compile_stream.grammar_module_name o
                (fun _ -> cparser_match _loc e po pcl)
          | None -> cparser_match _loc e po pcl} ]

    stream_exp :
    ["!"; `Uid(n) %{
      Ref.protect Compile_stream.grammar_module_name n (fun _ ->
           Compile_stream.empty _loc )}
    |  "!"; `Uid(n); stream_exp_comp_list{sel}  %{
        Ref.protect Compile_stream.grammar_module_name n (fun _ -> cstream _loc sel)}
    | stream_exp_comp_list{sel} %{ cstream _loc sel}
    |  %{  Compile_stream.empty _loc}
    ]

    
     parser_ipat :
     [ a_lident{i} %{ (i: alident:> pat)}
     | "_" %{ %pat{ _ }}
     ]         

     parser_case_list :
     ["|"; L0 parser_case SEP "|"{pcl} %{ pcl}
     ]
    
    parser_case :
    [stream_pat{sp}; "->"; exp{e} %{   (sp, None, e)}
    ] 
    (* stream_begin : *)
    (* [ "[<"; OPT [ "!"; `Uid(n)->n]{name} -> name  ]    *)

    stream_pat :
    [ stream_pat_comp{spc} %{ [(spc, None)]}
    | stream_pat_comp{spc}; ";"; stream_pat_comp_err_list{sp} %{ (spc, None) :: sp}
    | %{ [] }]


    stream_pat_comp : (* FIXME here *)
    [ pat{p}; "when"; exp{e}  %{  SpWhen (_loc, p, (Some e))}
    | pat{p} %{ SpWhen (_loc, p, None)}
    | pat{p}; "="; exp{e} %{ SpMatch (_loc, p, e)}
    | "'"; pat{p} %{ SpStr (_loc, p)}
    ]
    stream_pat_comp_err :
    [ stream_pat_comp{spc};   "??"; exp{e} %{  (spc, Some e)}
    | stream_pat_comp{spc} %{ (spc,None)}
    ] 

    stream_pat_comp_err_list :
    [ stream_pat_comp_err{spc} %{ [spc]}
    | stream_pat_comp_err{spc}; ";" %{ [spc]}
    | stream_pat_comp_err{spc}; ";"; stream_pat_comp_err_list{sp} %{ spc :: sp}
    ]

    stream_exp_comp : 
    [  exp{e} %{ SeTrm _loc e}
    | "'";exp{e} %{ SeNtr _loc e}
    ]

    stream_exp_comp_list :
    [ stream_exp_comp{se}; ";"; stream_exp_comp_list{sel} %{ se :: sel}
    | stream_exp_comp{se}; ";" %{ [se]}
    | stream_exp_comp{se} %{ [se]}
    ] 
};;


let fill_parsers =
  let  applied = ref false in
  fun () ->
    if not !applied then
      begin
        apply ();
        applied := true 
      end
        
let () = 
Ast_parsers.register_parser
    ("stream", fill_parsers);;























(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/parse_stream.cmo" *)
(* end: *)
