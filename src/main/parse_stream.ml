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
  stream_pat_comp stream_pat_comp_err 
  stream_pat_comp_err_list
  stream_pat parser_case parser_case_list 
}

(** Even though we did not using lexing convention to fully interleave foreign DDSL and 
    hot lanuage, we can still make it a DDSL as long as we don't use [unsafe_extend]
 *)  
let apply () = 
  %extend{
    let  uid: [`Uid(n) %{n}]
    exp : Level "top"
        [ "parser";  OPT uid  {name}; parser_case_list{pcl} %{
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
    [stream_pat{sp}; "->"; exp{e} %{   (sp, None, e)}
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


let fill_parsers =
  let  applied = ref false in
  fun () ->
    if not !applied then
      begin
        apply ();
        applied := true 
      end
        
let () =
  begin 
    Ast_parsers.register_parser
      ("stream", fill_parsers);
  end;;























(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/parse_stream.cmo" *)
(* end: *)
