(*
let f (loc:Locf.t) meta content  =
  let open! Syntaxf in
  let exp = exp  in
  Gramf.protect exp 
    %extend{
    exp : 10 RA 
    ["let"; "!"; opt_rec as r ; bind as bi ; "in"; exp as x %{`LetIn(_loc,r,bi,x)}]
    } (fun entry ->
      Gramf.parse_string_eoi entry ~loc content
      )


%protect{
  exp: 10 RA
  ["let"; "!"; opt_rec as r ; bind as bi ; "in"; exp as x %{`LetIn(_loc,r,bi,x)}]
  pat: 20 RA
  [...]
    
  ${
  Gramf.parse_string_eoi exp ~loc content
  }  
}    

let () =
  let d = Ns.lang in
  begin
    Ast_quotation.add
      {domain = d; name="cexp";}
      Dyn_tag.exp f;
  end
*)
    
(* local variables: *)
(* compile-command: "cd .. && pmake  main_annot/lang_monad.cmo" *)
(* end: *)
