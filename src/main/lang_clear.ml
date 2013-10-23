
%import{
Ast_gen:
  loc_of
  seq_sem
  ;

}
open FAst
  
%create{
a_lident
(nonterminalsclear : exp Gramf.t)
qualuid  
}
  
%extend{
a_lident :
  [ Ant(""|"lid" as n,s) %{FanUtil.mk_anti _loc  ~c:"a_lident" n s}
  | Lid s  %{ `Lid (_loc, s)} ]
nonterminalsclear :
  [ qualuid{t}; L1 a_lident {ls} %{
    ls
    |> List.map (fun (x:alident) ->
      let  x = (x:alident :> exp) in 
      let _loc = loc_of x in
      %exp{ $id:t.clear $x })
    |> seq_sem} ]
qualuid :
  [ Uid x; ".";  S{xs}  %ident'{$uid:x.$xs}
  | Uid x %{ `Uid(_loc,x)}
  ]   
}  

let _ =
  let d = Ns.lang in 
  begin
    Ast_quotation.of_exp
      ~name:(d,"clear") ~entry:nonterminalsclear ();
  end
(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lang_clear.cmo" *)
(* end: *)
