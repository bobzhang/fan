%import{
Compile_stream:
  cparser
  cstream
  ;
Syntaxf:
  exp
  a_lident
  pat
  ;
}

open FAst


%create{ 
  parser_ipat
  parser_exp
  stream_pat_comp stream_pat_comp_err 
  stream_pat_comp_err_list
  stream_pat parser_case parser_case_list 
};;

(** Even though we did not using lexing convention to fully interleave foreign DDSL and 
    hot lanuage, we can still make it a DDSL as long as we don't use [unsafe_extend]
 *)  

  %extend{
    let  uid: [Uid n %{n}]
    parser_exp : 
        [  ? uid  {name}; parser_case_list{pcl} %{
          match name with
          | Some o ->
              Ref.protect Compile_stream.grammar_module_name o (fun _ -> cparser _loc  pcl)
          | None -> cparser _loc  pcl}
        ]
     parser_ipat :
     [ a_lident{i} %{ (i: alident:> pat)}
     | "_" %{ %pat{ _ }}
     ]         

     parser_case_list :
     ["|"; L0 parser_case SEP "|"{pcl} %{ pcl}
     ]
    
    parser_case :
    [stream_pat{sp}
     (*  let e = *)
     (*    if x.name = Tokenf.empty_name then *)
     (*      let expander loc _ s = Gramf.parse_string ~loc Syntaxf.exp s in *)
     (*      Tokenf.quot_expand expander x *)
     (*    else Ast_quotation.expand x Dyn_tag.exp in *)
     (*  (sp,None, e) *)
     (* } *); "->"; exp{e} %{   (sp, None, e)}
    ] 
    stream_pat :
    [ stream_pat_comp{spc} %{ [(spc, None)]}
    | stream_pat_comp{spc}; ";"; stream_pat_comp_err_list{sp} %{ (spc, None) :: sp}
    | %{ [] }]


    stream_pat_comp : (* FIXME here *)
    [ pat{p}; "when"; exp{e}  %{  When (_loc, p, (Some e))}
    | pat{p} %{ When (_loc, p, None)}
    | pat{p}; "="; exp{e} %{ Match (_loc, p, e)}
    | "'"; pat{p} %{ Str (_loc, p)}
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
};;


        
let () =
  begin
    Ast_quotation.of_exp ~name:(Ns.lang,"parser" ) ~entry:parser_exp ()
  end;;























(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/parse_stream.cmo" *)
(* end: *)
