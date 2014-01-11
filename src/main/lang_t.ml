
open Syntaxf
open Astfn 
open Util 
let dispatch_tbl : (string, ctyp -> exp) Hashtbl.t = Hashtbl.create 31
let current_name : string option ref = ref None ;;


%create{t};;




(** register dispatcher based on name and
    %t@sexp{ (int * int) list }
 *)
%extend{
t:
  [ctyp as x %{
   match !current_name with
   | None -> failwith "No attribute attached to dsl t"
   | Some n ->
       begin
         let n = String.capitalize n in
         let try
           f  = Hashtbl.find dispatch_tbl n in
           Fill.exp _loc (f (Strip.ctyp x ))
         with 
           Not_found -> failwithf "%s not registered with t" n 
       end 
 }]
};;



let _ =
  let d = Ns.lang in
  let parser = Ast_quotation.make_parser ~lexer:Lex_fan.from_stream t in
  let f = fun loc meta content ->
    Ref.protect current_name meta (fun _ -> parser loc meta content) in
  let f2 = fun _loc meta content -> %stru{$exp{f _loc meta content}} in
  begin 
    Ast_quotation.add {domain = d; name = "t"} Dyn_tag.exp f ;
    Ast_quotation.add {domain = d; name = "t"} Dyn_tag.stru f2 ;
  end

  (* begin *)
  (*   %register{ *)
  (*   name:t; *)
  (*   position: exp ; *)
  (*   entry: t *)
  (* } *)
  (* end *)

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/lang_t.cmo" *)
(* end: *)
