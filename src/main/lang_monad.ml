open! Syntaxf
open Astf
(* TODO, inline optimizations ...
   [match!]
 *)

type t =  {
    bind : bind ;
    exp : exp
  } 
let specializer  : (string, ( t -> exp))Hashtbl.t =  Hashtbl.create 0


let _  = begin
  let (+>) = Hashtbl.add specializer in
  "Option" +> (fun t ->
    Ast_basic.fold_and_right
      (fun bind acc ->
      match bind with
      | %bind{ $p = $e } ->
          %exp{match ($e : _ option) with | Some $p -> $acc | None -> None }
          (* %exp{$uid:module_name.bind $e (fun $p -> $acc)} *)
      | _ -> assert false)
      t.bind t.exp)
end



    
let f (loc:Locf.t) meta content =
  let module_name =
    match meta with
    | None -> Locf.failf loc "cexp module name must be specified via \\@"
    | Some x -> String.capitalize x in 
  %local_extend{
    exp: 10 RA
    ["let"; "!" ; bind as bi ; "in"; exp as x %{
     let try f = Hashtbl.find specializer module_name in
     f {bind = bi; exp = x}
     with
     Not_found  -> 
     Ast_basic.fold_and_right
       (fun bind acc ->
         match bind with
         | %bind{ $p = $e } ->
             %exp{$uid:module_name.bind $e (fun $p -> $acc)}
         | _ -> assert false
       ) bi x }
   ]
    ${Gramlib.parse_string_eoi exp ~loc content}}

(*
%local_extend{
  %{
  let module_name =
    match annotation with
    | None -> Locf.failf loc "cexp module name must be specified via \\@"
    | Some x -> String.capitalize x}
    
  exp: 10 RA 
    ["let"; "!" ; bind as bi ; "in"; exp as x %{
     let try f = Hashtbl.find specializer module_name in
     f {bind = bi; exp = x}
     with
     Not_found  -> 
     Ast_basic.fold_and_right
       (fun bind acc ->
         match bind with
         | %bind{ $p = $e } ->
             %exp{$uid:module_name.bind $e (fun $p -> $acc)}
         | _ -> assert false
       ) bi x }
   ]
  %{
    entry: exp;
    position: exp, stru;
    name: cexp;            
   }               
}
*)    
    
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
