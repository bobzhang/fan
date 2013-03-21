open Ast
open LibUtil
type key = string 
let inject_expr_tbl: (key,exp) Hashtbl.t = Hashtbl.create 40
let inject_stru_tbl: (key,stru) Hashtbl.t = Hashtbl.create 40
let inject_cstru_tbl: (key,cstru) Hashtbl.t = Hashtbl.create 40
let register_inject_exp (k,f) = Hashtbl.replace inject_expr_tbl k f
let register_inject_stru (k,f) = Hashtbl.replace inject_stru_tbl k f
let register_inject_cstru (k,f) = Hashtbl.replace inject_cstru_tbl k f
let inject_exp = Gram.mk "inject_exp"
let inject_stru = Gram.mk "inject_stru"
let inject_cstru = Gram.mk "inject_cstru"
let _ =
  Gram.extend_single (inject_exp : 'inject_exp Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid x ->\n         ((try Hashtbl.find inject_expr_tbl x\n           with | Not_found  -> failwithf \"inject.exp %s not found\" x) : \n         'inject_exp )\n     | _ ->\n         failwith\n           \"try Hashtbl.find inject_expr_tbl x\nwith | Not_found  -> failwithf \"inject.exp %s not found\" x\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid x ->
                       ((try Hashtbl.find inject_expr_tbl x
                         with
                         | Not_found  ->
                             failwithf "inject.exp %s not found" x) : 
                       'inject_exp )
                   | _ ->
                       failwith
                         "try Hashtbl.find inject_expr_tbl x\nwith | Not_found  -> failwithf \"inject.exp %s not found\" x\n"))))]));
  Gram.extend_single (inject_stru : 'inject_stru Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid x ->\n         ((try Hashtbl.find inject_stru_tbl x\n           with | Not_found  -> failwithf \"inject.exp %s not found\" x) : \n         'inject_stru )\n     | _ ->\n         failwith\n           \"try Hashtbl.find inject_stru_tbl x\nwith | Not_found  -> failwithf \"inject.exp %s not found\" x\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid x ->
                       ((try Hashtbl.find inject_stru_tbl x
                         with
                         | Not_found  ->
                             failwithf "inject.exp %s not found" x) : 
                       'inject_stru )
                   | _ ->
                       failwith
                         "try Hashtbl.find inject_stru_tbl x\nwith | Not_found  -> failwithf \"inject.exp %s not found\" x\n"))))]));
  Gram.extend_single (inject_cstru : 'inject_cstru Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid x ->\n         ((try Hashtbl.find inject_cstru_tbl x\n           with | Not_found  -> failwithf \"inject.exp %s not found\" x) : \n         'inject_cstru )\n     | _ ->\n         failwith\n           \"try Hashtbl.find inject_cstru_tbl x\nwith | Not_found  -> failwithf \"inject.exp %s not found\" x\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid x ->
                       ((try Hashtbl.find inject_cstru_tbl x
                         with
                         | Not_found  ->
                             failwithf "inject.exp %s not found" x) : 
                       'inject_cstru )
                   | _ ->
                       failwith
                         "try Hashtbl.find inject_cstru_tbl x\nwith | Not_found  -> failwithf \"inject.exp %s not found\" x\n"))))]))
let _ =
  let open AstQuotation in
    of_exp ~name:((`Absolute ["Fan"; "Inject"]), "exp") ~entry:inject_exp;
    of_stru ~name:((`Absolute ["Fan"; "Inject"]), "stru") ~entry:inject_stru;
    of_cstru ~name:((`Absolute ["Fan"; "Inject"]), "cstru")
      ~entry:inject_cstru
