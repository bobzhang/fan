open Ast;
open LibUtil;

type key = string;

let inject_expr_tbl: Hashtbl.t key expr = Hashtbl.create 40; 
let inject_stru_tbl: Hashtbl.t key stru = Hashtbl.create 40;
let inject_cstru_tbl: Hashtbl.t key cstru = Hashtbl.create 40;

let register_inject_expr (k,f)=
  Hashtbl.replace inject_expr_tbl k f;
let register_inject_stru (k,f)=
  Hashtbl.replace inject_stru_tbl k f;
let register_inject_cstru (k,f) =
  Hashtbl.replace inject_cstru_tbl k f;



{:create|Gram
  inject_expr inject_stru inject_cstru
|};
  
{:extend| Gram
  inject_expr:
  [`Lid x ->
     try Hashtbl.find inject_expr_tbl x 
     with [Not_found -> failwithf "inject.expr %s not found" x ]]

  inject_stru:
  [`Lid x ->
     try Hashtbl.find inject_stru_tbl x
     with [Not_found -> failwithf "inject.expr %s not found" x ]]

  inject_cstru:
  [`Lid x ->
     try Hashtbl.find inject_cstru_tbl x
     with [Not_found -> failwithf "inject.expr %s not found" x ]]
|};

let open AstQuotation in begin
  of_expr
    ~name:((`Absolute ["Fan";"Inject"], "expr")) ~entry:inject_expr;
  of_stru
    ~name:((`Absolute ["Fan";"Inject"],"stru"))
    ~entry:inject_stru;
  of_cstru
    ~name:((`Absolute ["Fan";"Inject"], "cstru"))
     ~entry:inject_cstru; 
end;  
