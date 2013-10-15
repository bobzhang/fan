open FAst
open Util

type key = string

let inject_exp_tbl: (key,exp) Hashtbl.t = Hashtbl.create 40

let inject_stru_tbl: (key,stru) Hashtbl.t = Hashtbl.create 40
    
let inject_clfield_tbl: (key,clfield)Hashtbl.t = Hashtbl.create 40

let register_inject_exp (k,f)=
  Hashtbl.replace inject_exp_tbl k f
    
let register_inject_stru (k,f)=
  Hashtbl.replace inject_stru_tbl k f
    
let register_inject_clfield (k,f) =
  Hashtbl.replace inject_clfield_tbl k f
;;


%create2{inject_exp inject_stru inject_clfield};;
  
%extend2{
  inject_exp:
  [`Lid x %{
   try Hashtbl.find inject_exp_tbl x 
   with Not_found -> failwithf "inject.exp %s not found" x } ]

  inject_stru:
  [`Lid x %{
   try Hashtbl.find inject_stru_tbl x
   with Not_found -> failwithf "inject.exp %s not found" x }]

  inject_clfield:
  [`Lid x %{
   try Hashtbl.find inject_clfield_tbl x
   with Not_found -> failwithf "inject.exp %s not found" x }]
};;

let open Ast_quotation in
let d = Ns.inject in 
begin
  of_exp ~name:(d, "exp") ~entry:inject_exp ();
  of_stru ~name:(d,"stru") ~entry:inject_stru ();
  of_clfield ~name:(d, "clfield") ~entry:inject_clfield ();  
end

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/ast_inject.cmo" *)
(* end: *)
