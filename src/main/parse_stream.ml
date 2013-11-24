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
};;

open Astf


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
    parser_exp : 
        [  ? Uid n ; parser_case_list as pcl %{
          match n with
          | Some o ->
              Ref.protect Compile_stream.grammar_module_name o (fun _ -> cparser _loc  pcl)
          | None -> cparser _loc  pcl}
        ]
     parser_ipat :
     [ a_lident as i %{ (i: alident:> pat)}
     | "_" %{ %pat{ _ }}
     ]         

     parser_case_list :
     ["|"; L0 parser_case SEP "|" as pcl %{ pcl}
     ]
    
    parser_case :
    [stream_pat as sp
     (*  let e = *)
     (*    if x.name = Tokenf.empty_name then *)
     (*      let expander loc _ s = Gramf.parse_string ~loc Syntaxf.exp s in *)
     (*      Tokenf.quot_expand expander x *)
     (*    else Ast_quotation.expand x Dyn_tag.exp in *)
     (*  (sp,None, e) *)
     (* } *); "->"; exp as e %{   (sp, None, e)}
    ] 
    stream_pat :
    [ stream_pat_comp as spc %{ [(spc, None)]}
    | stream_pat_comp as spc; ";"; stream_pat_comp_err_list as sp %{ (spc, None) :: sp}
    | %{ [] }]


    stream_pat_comp : (* FIXME here *)
    [ pat as p; "when"; exp as e  %{  When (_loc, p, (Some e))}
    | pat as p %{ When (_loc, p, None)}
    | pat as p; "="; exp as e %{ Match (_loc, p, e)}
    | "'"; pat as p %{ Str (_loc, p)}
    ]
    stream_pat_comp_err :
    [ stream_pat_comp as spc;   "??"; exp as e %{  (spc, Some e)}
    | stream_pat_comp as spc %{ (spc,None)}
    ] 

    stream_pat_comp_err_list :
    [ stream_pat_comp_err as spc %{ [spc]}
    | stream_pat_comp_err as spc; ";" %{ [spc]}
    | stream_pat_comp_err as spc; ";"; stream_pat_comp_err_list as sp %{ spc :: sp}
    ]
};;


        
let () =
  begin
    Ast_quotation.of_exp ~name:{domains =  Ns.lang; name = "parser" } ~entry:parser_exp ()
  end;;























(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/parse_stream.cmo" *)
(* end: *)
