open! Syntaxf

(* TODO, inline optimizations ...
   [match!]
 *)
let f (loc:Locf.t) meta content =
  let module_name =
    match meta with
    | None -> Locf.failf loc "cexp module name must be specified via @"
    | Some x -> String.capitalize x in 
  %local_extend{
    exp: 10 RA
    ["let"; "!" ; bind as bi ; "in"; exp as x %{
     Ast_basic.fold_and_right
       (fun bind acc ->
         match bind with
         | %bind{ $p = $e } ->
             %exp{$uid:module_name.bind $e (fun $p -> $acc)}
         | _ -> assert false
       ) bi x }]
    ${Gramlib.parse_string_eoi exp ~loc content}}

let f2 (_loc:Locf.t) _meta content = 
  let res = f _loc _meta content in
  %stru{$exp:res}
    
let () =
  let d = Ns.lang in
  begin
    Ast_quotation.add {domain = d; name="cexp";} Dyn_tag.exp f;
    Ast_quotation.add {domain = d; name="cexp";} Dyn_tag.stru f2;
  end

    
(* local variables: *)
(* compile-command: "cd .. && pmake  main_annot/lang_monad.cmo" *)
(* end: *)
