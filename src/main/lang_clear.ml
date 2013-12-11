
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
      let  x = (x:alident :> exp) in 
      let _loc = loc_of x in
      %exp{ $id:t.clear $x })
    |> seq_sem} ]
qualuid :
  [ Uid x; ".";  S as xs  %ident'{$uid:x.$xs}
  | Uid x %{ `Uid(_loc,x)}
  ]   
}  

let _ =
  let d = Ns.lang in 
  begin
    Ast_quotation.of_exp
      ~name:{domain = d; name = "clear"} ~entry:nonterminalsclear ();
  end

(*
    
%extend{
%create{
  a_lident
  (nonterminalsclear : exp Gramf.t)
  qualuid  
  }  
a_lident :
  [ Ant(""|"lid",s) %{Tokenf.mk_ant ~c:"a_lident" s}
  | Lid s  %{ `Lid (_loc, s)} ]
nonterminalsclear :
  [ qualuid as t; L1 a_lident as ls %{
    ls
    |> List.map (fun (x:alident) ->
      let  x = (x:alident :> exp) in 
      let _loc = loc_of x in
      %exp{ $id:t.clear $x })
    |> seq_sem} ]
qualuid :
  [ Uid x; ".";  S as xs  %ident'{$uid:x.$xs}
  | Uid x %{ `Uid(_loc,x)}
  ]
}
%register{
  entry: nonterminalsclear;
  position: exp ;
  name: clear;
  domain: ${Ns.lang}
  
}
%cexp@list{
  let! x = [3;5] in
  let! y = [2;3] in
  [x ;y]
  }
*)    
(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lang_clear.cmo" *)
(* end: *)
