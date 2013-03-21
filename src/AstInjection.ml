open Ast;
open LibUtil;

type key = string;

let inject_exp_tbl: Hashtbl.t key exp = Hashtbl.create 40; 
let inject_stru_tbl: Hashtbl.t key stru = Hashtbl.create 40;
let inject_cstru_tbl: Hashtbl.t key cstru = Hashtbl.create 40;

let register_inject_exp (k,f)=
  Hashtbl.replace inject_exp_tbl k f;
let register_inject_stru (k,f)=
  Hashtbl.replace inject_stru_tbl k f;
let register_inject_cstru (k,f) =
  Hashtbl.replace inject_cstru_tbl k f;



{:create|Gram
  inject_exp inject_stru inject_cstru
|};
  
{:extend| Gram
  inject_exp:
  [`Lid x ->
     try Hashtbl.find inject_exp_tbl x 
     with [Not_found -> failwithf "inject.exp %s not found" x ]]

  inject_stru:
  [`Lid x ->
     try Hashtbl.find inject_stru_tbl x
     with [Not_found -> failwithf "inject.exp %s not found" x ]]

  inject_cstru:
  [`Lid x ->
     try Hashtbl.find inject_cstru_tbl x
     with [Not_found -> failwithf "inject.exp %s not found" x ]]
|};

let open AstQuotation in begin
  of_exp
    ~name:((`Absolute ["Fan";"Inject"], "exp")) ~entry:inject_exp;
  of_stru
    ~name:((`Absolute ["Fan";"Inject"],"stru"))
    ~entry:inject_stru;
  of_cstru
    ~name:((`Absolute ["Fan";"Inject"], "cstru"))
     ~entry:inject_cstru; 
end;  
