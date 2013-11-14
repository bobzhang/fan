%import{
Compile_stream:
  cstream
  ;
Syntaxf:
  exp
  ;
};;



%create{
stream_exp stream_exp_comp stream_exp_comp_list
};;
  
%extend{
    stream_exp :
    ["!"; Uid n %{
      Ref.protect Compile_stream.grammar_module_name n (fun _ ->
           Compile_stream.empty _loc )}
    |  "!"; Uid n; stream_exp_comp_list as sel  %{
        Ref.protect Compile_stream.grammar_module_name n (fun _ -> cstream _loc sel)}
    | stream_exp_comp_list as sel %{ cstream _loc sel}
    |  %{  Compile_stream.empty _loc}
    ]
    stream_exp_comp : 
    [  exp as e %{ (Trm(_loc, e) : Compile_stream.sexp_comp)}
    | "'";exp as e %{ Ntr (_loc, e)}
    ]

    stream_exp_comp_list :
    [ stream_exp_comp as se; ";"; S as sel %{ se :: sel}
    | stream_exp_comp as se; ";" %{ [se]}
    | stream_exp_comp as se %{ [se]}
    ] 

}  

let _ = begin
  Ast_quotation.of_exp ~name:(Ns.lang,"stream") ~entry:stream_exp ()
end
(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lang_stream.cmo" *)
(* end: *)
