open Syntaxf
%create{code};;


let code_name : string ref = ref ""
;;


%extend{
code:
[ strus as x %{
  let code = (new Metafn.meta)# stru  _loc (Strip.stru x) in
  let name = "code_of_" ^ !code_name in 
  %stru{ $x;; let $lid:name = ( $code : Astfn.stru) } }]
}


(* @[serizal] *)
(* let f x  = x *)

(* @[serial] *)
(* type u = int *)

(* %serial{ *)

(* let f x = x *)
(* let f x = x  *)
(* let f y = y            *)
(* let u z = z *)
(* } *)
  
let _ =
  let parser = Ast_quotation.make_parser ~lexer:Lex_fan.from_stream code in 
  Ast_quotation.add {domain = Ns.lang; name = "code"} Dyn_tag.stru begin fun loc meta s -> 
    match meta with
    | None -> parser loc meta s 
    | Some x -> Ref.protect code_name x (fun _ ->  parser loc meta s )
  end




(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lang_code.cmo" *)
(* end: *)
