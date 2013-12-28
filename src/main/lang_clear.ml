
%import{
Ast_gen:
  loc_of
  seq_sem
  ;

};;

open Astf
  
%create{
a_lident
(nonterminalsclear : exp Gramf.t)
qualuid  
};;
  
%extend{
a_lident :
  [ Ant(""|"lid",s) %{Tokenf.mk_ant ~c:"a_lident" s}
  | Lid s  %{ `Lid (_loc, s)} ]
nonterminalsclear :
  [ qualuid as t; L1 a_lident as ls %{
    ls
    |> List.map (fun (x:alident) ->
      let _loc = loc_of x in
      %exp{ $id:t.clear $x })
    |> seq_sem} ]
qualuid :
  [ Uid x; ".";  S as xs  %ident'{$uid:x.$xs}
  | Uid x %{ `Uid(_loc,x)}
  ]   
}  

let _ =
  %register{
  entry: nonterminalsclear;
  position: exp ;
  name: clear}

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lang_clear.cmo" *)
(* end: *)
